;;; tabby.el --- An unofficial tabby plugin for Emacs  -*- lexical-binding:t -*-

;; Author: Alan Wong <heywym@qq.com>
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.2") (s "1.12.0"))
;; Keywords: tabby, completion, llm, copilot
;; URL: http://github.com/alan-w-255/tabby.el

;;; License:

;; This file is part of tabby.el.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This package provides an unofficial tabby plugin for Emacs.

;;; Code:
(require 'json)
(require 'cl-lib)
(require 's)

(defgroup tabby nil
  "Tabby."
  :group 'completion
  :prefix "tabby-")

(defconst tabby-version "1.3.2"
  "Tabby version.")

;; custom

(defcustom tabby-log-max 10000
  "Max size of events buffer. 0 disables, nil means infinite.
Enabling event logging may slightly affect performance."
  :group 'tabby
  :type 'integer)

(defcustom tabby-clear-overlay-ignore-commands nil
  "List of commands that should not clear the overlay when called."
  :group 'tabby
  :type '(repeat function))

(defcustom tabby-enable-predicates nil
  "A list of predicate functions with no argument to enable Tabby.
Tabby will be triggered only if all predicates return t."
  :group 'tabby
  :type '(repeat function))

(defcustom tabby-disable-predicates nil
  "A list of predicate functions with no argument to disable Tabby.
Tabby will not be triggered if any predicate returns t."
  :type '(repeat function)
  :group 'tabby)

(defcustom tabby-idle-delay 0.2
  "Time in seconds to wait before starting completion.

Complete immediately if set to 0.
Disable idle completion if set to nil."
  :type '(choice
          (number :tag "Seconds of delay")
          (const :tag "Idle completion disabled" nil))
  :group 'tabby)

(defcustom tabby-enable-display-predicates nil
  "A list of predicate functions with no argument to enable Tabby.
Tabby will show completions only if all predicates return t."
  :type '(repeat function)
  :group 'tabby)

(defcustom tabby-disable-display-predicates nil
  "A list of predicate functions with no argument to disable Tabby.
Tabby will not show completions if any predicate returns t."
  :type '(repeat function)
  :group 'tabby)

(defcustom tabby-major-mode-alist '(("rustic" . "rust")
                                    ("cperl" . "perl")
                                    ("c++" . "cpp")
                                    ("clojurec" . "clojure")
                                    ("clojurescript" . "clojure")
                                    ("objc" . "objective-c")
                                    ("cuda" . "cuda-cpp")
                                    ("docker-compose" . "dockercompose")
                                    ("coffee" . "coffeescript")
                                    ("js" . "javascript")
                                    ("js2" . "javascript")
                                    ("js2-jsx" . "javascriptreact")
                                    ("typescript-tsx" . "typescriptreact")
                                    ("rjsx" . "typescriptreact")
                                    ("less-css" . "less")
                                    ("text" . "plaintext")
                                    ("ess-r" . "r")
                                    ("enh-ruby" . "ruby")
                                    ("shell-script" . "shellscript")
                                    ("sh" . "shellscript")
                                    ("visual-basic" . "vb")
                                    ("nxml" . "xml"))
  "Alist mapping major mode names (with -mode removed) to Tabby language ID's."
  :type '(alist :key-type string :value-type string)
  :group 'tabby)

;; vars

(defconst tabby--base-dir
  (file-name-directory
   (or load-file-name
       (buffer-file-name)))
  "Directory containing this file.")

(defvar-local tabby--overlay nil
  "Overlay for tabby completion.")

(defvar tabby--connection nil
  "Tabby agent jsonrpc connection instance.")

(defvar tabby--request-id 0
  "Request id.")

(defvar tabby-mode-map (make-sparse-keymap)
  "Keymap for Tabby minor mode.
Use this for custom bindings in `tabby-mode'.")

(defvar tabby-trigger-mode "auto"
  "Trigger mode.")

(defvar tabby--current-completion-request nil
  "Current completion request.")

(defvar tabby--current-completion-response nil
  "Current completion response.")

(defvar tabby--agent-status nil
  "Tabby agent status.")

(defvar tabby--agent-issue nil
  "Tabby agent issue.")

(defvar tabby--ongoing-request-id 0
  "Ongoing request id.")

(defvar tabby--agent-request-callback-alist nil
  "Alist mapping request id's to callbacks.")

(defvar tabby--status "initialization_done")

(defvar tabby--post-command-timer nil
  "Timer for tabby completion.")

;; utils

(defsubst tabby--connection-alivep ()
  "Non-nil if the `tabby--connection' is alive."
  (and tabby--connection
       (process-live-p tabby--connection)))

(defconst tabby--ignore-response
  (lambda (_))
  "Simply ignore the response.")

(defun tabby--next-request-id ()
  "Get the next request id."
  (setq tabby--request-id (1+ tabby--request-id))
  tabby--request-id)

(defun tabby--get-client-properties ()
  "Get client properties."
  `(:user
    (:emacs (:triggerMode ,tabby-trigger-mode))
    :session
    (:client ,(car (split-string (emacs-version) "\n"))
             :ide (:name "Emacs" :version ,(car (split-string emacs-version "\n")))
             :tabby_plugin (:name "TabbyML/emacs-tabby" :version ,tabby-version))))

(defun tabby--request (args &optional cb)
  "Send a request to the tabby agent with ARGS. CB is called with the response."
  (unless (tabby--connection-alivep)
    (tabby--agent-start))
  (let* ((id (tabby--next-request-id))
         (request (list id args))
         (buf (current-buffer))
         (on-success (lambda (response)
                       (when cb
                         (with-current-buffer buf
                           (funcall cb response))))))
    (push (cons id on-success) tabby--agent-request-callback-alist)
    (process-send-string tabby--connection (concat (json-encode request) "\n"))
    id))

(defun tabby--get-language ()
  "Get language of current buffer."
  (let ((mode (replace-regexp-in-string "-mode\\'\\|-ts-mode\\'" "" (symbol-name major-mode))))
    (alist-get mode tabby-major-mode-alist mode nil 'equal)))

;;; agent

(defvar deepseek-bearer-token (auth-source-pick-first-password :host "api.deepseek.com" :user "alan"))

(defvar test-response-data nil)

(defun handle-response (status id)
  (goto-char (point-min))
  (re-search-forward "^$")
  (delete-region (point-min) (point))
  (let ((response (json-read)))
    (setq test-response-data `((fim-id . ,id) ,response))))

(fset 'deepseek-get-fim-id
      ((lambda ()
         (let ((_id -1))
           (lambda ()
             (setq _id (1+ _id))
             _id)))))

;; (deepseek-get-fim-id)

(defun deepcoder-fim ()
  (let* ((url "https://api.deepseek.com/beta/completions")
         (headers `(("Content-Type" . "application/json")
                    ("Accept" . "application/json")
                    ("Authorization" . ,(format "Bearer %s" deepseek-bearer-token))))
         (data (json-encode '((model . "deepseek-chat")
                              (prompt . "Once upon a time, ")
                              (echo . :json-false))))
         (url-request-method "POST")
         (url-request-extra-headers headers)
         (url-request-data data))
    (url-retrieve url 'handle-response `(,(deepseek-get-fim-id)))))


;; (deepcoder-fim)


;;; completion

(defun tabby--get-completion-context ()
  "Get completion context."
  `(:filepath
    ,(buffer-file-name)
    :prompt
    ,(buffer-substring-no-properties (point-min) (point))
    :suffix
    ,(buffer-substring-no-properties (point) (point-max))))

(defun tabby-complete ()
  "Do completion."
  (interactive)
  (when (called-interactively-p 'any)
    (setq is-manual t))
  (when (string= tabby--status "initialization_done")
    (if (not (zerop tabby--ongoing-request-id))
        (tabby--agent-cancel-request tabby--ongoing-request-id)
      (let* ((request (tabby--get-completion-context)))
        (setq tabby--current-completion-request request)))))

(defun tabby--handle-completion-response (response)
  "Handle completion response."
  (when (eql tabby--ongoing-request-id (assq 'completion-id response))
    (setq tabby--ongoing-request-id 0)
    (when-let* ((completion-id (assq 'completion-id response))
                (text (cdr (assq 'text
                                 (aref
                                  (cdr (assq 'choices
                                             (cadr response)))
                                  0)))))
      (setq tabby--current-completion-response text)
      (tabby--overlay-show-completion `((completion-id . ,completion-id)
                                        (text . ,text))))))


(defun tabby-dismiss ()
  "Dismiss completion."
  (interactive)
  (when tabby--current-completion-request
    (setq tabby--current-completion-request nil)
    (tabby--clear-overlay)
    (when-let* ((response tabby--current-completion-response)
                (completion-id (cadr response))
                (choices (plist-get (cadr response) :choices))
                (choice (car choices)))
      (setq tabby--current-completion-response nil))))

(defmacro tabby--satisfy-predicates (enable disable)
  "Return t if satisfy all predicates in ENABLE and none in DISABLE."
  `(and (cl-every (lambda (pred)
                    (if (functionp pred) (funcall pred) t))
                  ,enable)
        (cl-notany (lambda (pred)
                     (if (functionp pred) (funcall pred) nil))
                   ,disable)))

(defun tabby--satisfy-trigger-predicates ()
  (tabby--satisfy-predicates tabby-enable-predicates tabby-disable-predicates))


;;; ui

(defface tabby-overlay-face
  '((t :inherit shadow))
  "Face for tabby overlay")

(defun tabby--overlay-visible ()
  "Return t if the overlay is visible."
  (and (overlayp tabby--overlay)
       (overlay-buffer tabby--overlay)))

(defun tabby-current-completion ()
  "Get current completion."
  (and (tabby--overlay-visible)
       (overlay-get tabby--overlay 'completion)))

(defconst tabby-completion-map (make-sparse-keymap)
  "Keymap for Tabby completion overlay.")

(defun tabby--get-overlay ()
  "Create or get overlay for tabby."
  (unless (overlayp tabby--overlay)
    (setq tabby--overlay (make-overlay 1 1 nil nil t))
    (overlay-put tabby--overlay 'keymap tabby-completion-map))
  tabby--overlay)

(defun tabby--clear-overlay ()
  "Clear overlay."
  (interactive)
  (when (tabby--overlay-visible)
    (delete-overlay tabby--overlay)))

(defun tabby--self-insert (command)
  "Handle the case where the char just inserted is the start of the completion.
If so, update the overlays and continue. COMMAND is the
command that triggered `post-command-hook'."
  (when (and (eq command 'self-insert-command)
             (tabby--overlay-visible)
             (tabby--satisfy-display-predicates))
    (let* ((ov tabby--overlay)
           (completion (overlay-get ov 'completion)))
      ;; The char just inserted is the next char of completion
      (when (eq last-command-event (elt completion 0))
        (if (= (length completion) 1)
            ;; If there is only one char in the completion, accept it
            (progn
              (overlay-put ov 'completion "")
              (tabby-accept-completion))
          (tabby--set-overlay-text ov (substring completion 1)))))))

(defun tabby--set-overlay-text (ov completion)
  "Set overlay OV with COMPLETION."
  (save-restriction
    (widen)
    (let* ((p-completion (propertize completion 'face 'tabby-overlay-face)))
      (add-text-properties 0 1 '(cursor 1) p-completion)
      (overlay-put ov 'after-string p-completion)
      (overlay-put ov 'display (char-to-string ?\u200B)) ;; \u200B is a zero-width space. Trick to fix the wrong character inserted position.
      (move-overlay ov (point) (point))
      (overlay-put ov 'completion completion))))

(defun tabby--overlay-show-completion (completion)
  "Render overlay."
  (tabby--clear-overlay)
  (when-let* ((completion-id (assq 'id completion))
              (text (assq 'text completion)))
    (overlay-put ov 'replace-end (+ (point) (length text)))
    (overlay-put ov 'completion-id (plist-get (cadr response) :id))
    (tabby--set-overlay-text ov text)))

(defun tabby-accept-completion (&optional transform-fn)
  "Accept completion. Return t if there is a completion.
Use TRANSFORM-FN to transform completion if provided."
  (interactive)
  (when (tabby--overlay-visible)
    (let* ((completion (overlay-get tabby--overlay 'completion))
           (_replace-end (overlay-get tabby--overlay 'replace-end))
           (suffix-replace-chars (overlay-get tabby--overlay 'suffix-replace-chars))
           (completion-id (overlay-get tabby--overlay 'completion-id))
           (choice-index (overlay-get tabby--overlay 'choice-index))
           (t-completion (funcall (or transform-fn 'identity) completion)))
      (tabby--agent-post-event
       `(:type
         "select"
         :completion_id
         ,completion-id
         :choice_index
         ,choice-index))
      (tabby--clear-overlay)
      (delete-region (point) (+ (point) suffix-replace-chars))
      (insert t-completion)
      ;; if it is a partial completion
      (when (and (s-prefix-p t-completion completion)
                 (not (s-equals-p t-completion completion)))
        (let ((ov (tabby--get-overlay))
              (suffix-len (if (< (+ (length t-completion) suffix-replace-chars) (length completion))
                              suffix-replace-chars
                            (- (length completion) (length t-completion)))))
          (when (< suffix-len 0)
            (setq suffix-len 0))
          (overlay-put ov 'suffix-replace-chars suffix-len)
          (tabby--set-overlay-text ov (s-chop-prefix t-completion completion))))
      t)))

(defmacro tabby--define-accept-completion-by-action (func-name action)
  "Define function FUNC-NAME to accept completion by ACTION."
  `(defun ,func-name (&optional n)
     (interactive "p")
     (setq n (or n 1))
     (tabby-accept-completion (lambda (completion)
                                (with-temp-buffer
                                  (insert completion)
                                  (goto-char (point-min))
                                  (funcall ,action n)
                                  (buffer-substring-no-properties (point-min) (point)))))))

(tabby--define-accept-completion-by-action tabby-accept-completion-by-word #'forward-word)
(tabby--define-accept-completion-by-action tabby-accept-completion-by-line #'forward-line)


;;; tabby-mode

(defun tabby--mode-enter ()
  "Set up tabby mode when entering."
  (add-hook 'post-command-hook 'tabby--post-command nil 'local))

(defun tabby--mode-exit ()
  "Clean up tabby mode when exiting."
  (remove-hook 'post-command-hook #'tabby--post-command 'local))

(defun tabby--satisfy-display-predicates ()
  "Return t if all display predicates are satisfied."
  (tabby--satisfy-predicates tabby-enable-display-predicates tabby-disable-display-predicates))

(defun tabby-turn-on-unless-buffer-read-only ()
  "Turn on `tabby-mode' if the buffer is writable."
  (unless buffer-read-only
    (tabby-mode 1)))


(defun tabby--post-command-debounce (buffer)
  "Complete in BUFFER."
  (when (and (buffer-live-p buffer)
             (equal (current-buffer) buffer)
             tabby-mode
             (tabby--satisfy-trigger-predicates))
    (tabby-complete)))


(defun tabby--post-command ()
  "Complete in `post-command-hook' hook."
  (when (and this-command
             (not (and (symbolp this-command)
                       (or
                        (s-starts-with-p "tabby-" (symbol-name this-command))
                        (member this-command tabby-clear-overlay-ignore-commands)
                        (tabby--self-insert this-command)))))
    (tabby-dismiss)
    (when tabby--post-command-timer
      (cancel-timer tabby--post-command-timer))
    (setq tabby--post-command-timer
          (run-with-idle-timer tabby-idle-delay
                               nil
                               'tabby--post-command-debounce
                               (current-buffer)))))

(advice-add 'keyboard-quit :after #'tabby-dismiss)

;;;###autoload
(define-minor-mode tabby-mode
  "Minor mode for Tabby."
  :init-value nil
  :lighter " Tabby"
  (tabby-dismiss)
  (if tabby-mode
      (tabby--mode-enter)
    (tabby--mode-exit)))

(provide 'tabby)
;;; tabby.el ends here
