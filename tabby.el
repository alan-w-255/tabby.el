;;; tabby.el --- An unofficial tabby plugin for Emacs  -*- lexical-binding:t -*-

;; Author: Alan Wong <heywym@qq.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.2") (s "1.12.0") (dash "2.19.1"))
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
(require 'dash)

(defgroup tabby nil
  "Tabby."
  :group 'completion
  :prefix "tabby-")

(defcustom tabby-log-max 10000
  "Max size of events buffer. 0 disables, nil means infinite.
Enabling event logging may slightly affect performance."
  :group 'tabby
  :type 'integer)

(defcustom tabby-node-executable
  (if (eq system-type 'windows-nt)
      "node.exe"
    "node")
  "Node executable path."
  :group 'tabby
  :type 'string)

(defcustom tabby-max-char 15000
  "Maximum number of characters to send to Tabby server."
  :group 'tabby
  :type 'integer)

(defconst tabby-version "1.2.0"
  "Tabby version.")

(defconst tabby--base-dir
  (file-name-directory
   (or load-file-name
       (buffer-file-name)))
  "Directory containing this file.")

(defvar-local tabby--overlay nil
  "Overlay for tabby completion.")

(defvar tabby--connection nil
  "Tabby agent jsonrpc connection instance.")

(defsubst tabby--connection-alivep ()
  "Non-nil if the `tabby--connection' is alive."
  (and tabby--connection
       (process-live-p tabby--connection)))

(defvar tabby--request-id 0)

(defun tabby--next-request-id ()
  (setq tabby--request-id (1+ tabby--request-id))
  tabby--request-id)

(defconst tabby--ignore-response
  (lambda (_))
  "Simply ignore the response.")

(defvar tabby-trigger-mode "auto"
  "Trigger mode.")

(defun tabby--get-client-properties ()
  "Get client properties."
  `(:user
    (:emacs (:triggerMode ,tabby-trigger-mode))
    :session
    (:client ,(car (string-split (emacs-version) "\n"))
	     :ide (:name "Emacs" :version ,(car (split-string emacs-version "\n")))
	     :tabby_plugin (:name "TabbyML/emacs-tabby" :version ,tabby-version))))


;;; agent
(defvar tabby--agent-status nil
  "Tabby agent status.")

(defvar tabby--agent-issue nil
  "Tabby agent issue.")

(defvar tabby--agent-request-callback-alist nil
  "Alist mapping request id's to callbacks.")

(defun tabby--request (args &optional cb)
  "Send a request to the tabby agent with ARGS."
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

(defun tabby--agent-start ()
  "Start the tabby agent process in local."
  (interactive)
  (if (not (locate-file tabby-node-executable exec-path))
      (user-error "Could not find node executable")
    (let ((node-version (->> (with-output-to-string
                               (call-process tabby-node-executable nil standard-output nil "--version"))
                             (s-trim)
                             (s-chop-prefix "v")
                             (string-to-number))))
      (cond ((< node-version 18)
             (user-error "Node 18+ is required but found %s" node-version))
            (t
             (setq tabby--connection
                   (make-process :name "tabby agent"
				 :buffer (get-buffer-create "*tabby agent*")
                                 :command (list tabby-node-executable
                                                (concat tabby--base-dir "node_scripts/tabby-agent.js"))
                                 :coding 'utf-8-emacs-unix
                                 :connection-type 'pipe
                                 :stderr (get-buffer-create "*tabby stderr*")
				 :filter 'tabby--agent-connection-filter
				 :sentinel 'tabby--agent-connection-sentinel
                                 :noquery t))
             (message "Tabby agent started.")
             (tabby--agent-initialize))))))

(defmacro tabby--ensure-connection-alive (&rest body)
  `(progn
     (unless (tabby--connection-alivep)
       (tabby--agent-start))
     (when (tabby--connection-alivep)
       ,@body)))

(defun tabby-agent-close ()
  "Close the tabby agent process."
  (interactive)
  (tabby--ensure-connection-alive
   (delete-process tabby--connection)
   (setq tabby--connection nil)
   (setq tabby--agent-status "exited")
   (setq tabby--request-id 0)))

(defun tabby--agent-on-error (data)
  (message "Tabby agent error: %s" data))

(defun tabby--agent-on-exit (data)
  (setq tabby--connection nil)
  (setq tabby--agent-status "exited")
  (message "Tabby agent exited: %s" data))

(defun tabby--agent-initialize ()
  (tabby--ensure-connection-alive
   (tabby--request `(:func initialize :args [(:clientProperties ,(tabby--get-client-properties))]))))

(defun tabby--agent-provide-completions (request cb)
  (tabby--ensure-connection-alive
   (tabby--request `(:func
		     provideCompletions
		     :args
		     [,request :signal t]) cb)))

(defvar tabby--ongoing-request-id 0
  "Ongoing request id.")

(defun tabby--agent-cancel-request (request-id)
  (tabby--ensure-connection-alive
   (tabby--request `(:func cancelRequest :args [,request-id]))
   (setq tabby--ongoing-request-id 0)))

(defun tabby--agent-post-event (event)
  (tabby--ensure-connection-alive
   (tabby--request `(:func postEvent :args [,event]))))

(defun tabby--agent-handle-response (response)
  (when-let ((data (cadr response))
	     (event (plist-get data :event)))
    (cl-case event
      ("statusChanged"
       (setq tabby--agent-status (plist-get data :status)))
      ("issueChanged"
       (setq tabby--agent-issue (plist-get data :issue)))))
  (when-let* ((id (car response))
	      (cb (alist-get id tabby--agent-request-callback-alist)))
    (funcall cb response)
    (setq tabby--agent-request-callback-alist (assq-delete-all id tabby--agent-request-callback-alist))))

(defun tabby--agent-connection-filter (process string)
  "Filter for tabby agent PROCESS."
  (with-current-buffer (process-buffer process)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert string)
      (when-let ((parsed (ignore-errors (json-parse-string string :object-type 'plist :array-type 'list))))
	(tabby--agent-handle-response parsed)))))

(defun tabby--agent-connection-sentinel (_proc event)
  (if (or (string= event "finished\n")
	  (string= event "deleted\n"))
      (setq tabby--agent-status "exited"))
  (if (string-match "failed with code -?[0-9]+\n" event)
      ;; todo
      (error event)))


;;; completion
(defvar tabby--position-offset 0
  "Position offset.")

(defvar tabby-major-mode-alist '(("rustic" . "rust")
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
  "Alist mapping major mode names (with -mode removed) to Tabby language ID's.")

(defun tabby--get-language ()
  "Get language of current buffer."
  (let ((mode (replace-regexp-in-string "-mode\\'\\|-ts-mode\\'" "" (symbol-name major-mode))))
    (alist-get mode tabby-major-mode-alist mode nil 'equal)))

(defun tabby--get-completion-context (is-manual)
  "Get completion context."
  (let ((half-tabby-max-char (/ tabby-max-char 2))
	(pos (1- (point)))
	text)
    (cond ((< (point-max) tabby-max-char)
	   (setq text (buffer-substring-no-properties (point-min) (point-max)))
	   (setq tabby--position-offset 0))
	  ((< (point) half-tabby-max-char)
	   (setq text (buffer-substring-no-properties (point-min) tabby-max-char))
	   (setq tabby--position-offset 0))
	  ((> (+ (point) half-tabby-max-char) (point-max))
	   (setq text (buffer-substring-no-properties (- (point-max) tabby-max-char) (point-max)))
	   (setq tabby--position-offset (- (point-max) tabby-max-char 1))
	   (setq pos (- (point) tabby--position-offset 1)))
	  (t
	   (setq text (buffer-substring-no-properties (- (point) half-tabby-max-char) (+ (point) half-tabby-max-char)))))
    `(:filepath
      ,(buffer-file-name)
      :language
      ,(tabby--get-language)
      :text
      ,text
      :position
      ,pos
      :manually
      ,(if is-manual
	   t
	 :json-false))))

(defvar tabby--current-completion-request nil
  "Current completion request.")

(defvar tabby--current-completion-response nil
  "Current completion response.")

(defvar tabby--status "initialization_done")

(defun tabby-complete (&optional is-manual)
  (interactive)
  (when (called-interactively-p 'any)
    (setq is-manual t))
  (when (string= tabby--status "initialization_done")
    (if (not (zerop tabby--ongoing-request-id))
	(tabby--agent-cancel-request tabby--ongoing-request-id)
      (let* ((request (tabby--get-completion-context is-manual))
	     (on-response (lambda (response)
			    (tabby--handle-completion-response request response))))
	(setq tabby--current-completion-request request)
	(setq tabby--ongoing-request-id
	      (tabby--agent-provide-completions request on-response))))))

(defun tabby--handle-completion-response (request response)
  (when (eql tabby--ongoing-request-id (car response))
    (setq tabby--ongoing-request-id 0)
    (when-let* ((choices (plist-get (cadr response) :choices))
		(choice (car choices)))
      (setq tabby--current-completion-response response)
      (tabby--overlay-show-completion request response)
      (tabby--agent-post-event
       `(:type
	 "view"
	 :completion_id
	 ,(plist-get (cadr response) :id)
	 :choice_index
	 ,(plist-get choice :index))))))

(defun tabby-dismiss ()
  (interactive)
  (setq tabby--current-completion-request nil)
  (tabby--clear-overlay))

(defcustom tabby-clear-overlay-ignore-commands nil
  "List of commands that should not clear the overlay when called."
  :group 'tabby
  :type '(repeat function))

(defcustom tabby-enable-predicates '(evil-insert-state-p)
  "A list of predicate functions with no argument to enable Tabby.
Tabby will be triggered only if all predicates return t."
  :type '(repeat function)
  :group 'tabby)

(defcustom tabby-disable-predicates nil
  "A list of predicate functions with no argument to disable Tabby.
Tabby will not be triggered if any predicate returns t."
  :type '(repeat function)
  :group 'tabby)

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

(defvar tabby-mode-map (make-sparse-keymap)
  "Keymap for Tabby minor mode.
Use this for custom bindings in `tabby-mode'.")

;;;###autoload
(define-minor-mode tabby-mode
  "Minor mode for Tabby."
  :init-value nil
  :lighter " Tabby"
  (tabby-dismiss)
  (if tabby-mode
      (tabby--mode-enter)
    (tabby--mode-exit)))

(defun tabby--post-command-debounce (buffer)
  "Complete in BUFFER."
  (when (and (buffer-live-p buffer)
             (equal (current-buffer) buffer)
	     tabby-mode
	     (tabby--satisfy-trigger-predicates))
    (tabby-complete)))

;;; ui

(defface tabby-overlay-face
  '((t :inherit shadow))
  "Face for tabby overlay")

(defun tabby--overlay-visible ()
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
    (let* ((suffix-replace-chars (overlay-get ov 'suffix-replace-chars))
	   (p-completion (concat (propertize completion 'face 'tabby-overlay-face)
				 (if (eobp)
				     ""
				   (buffer-substring
				    (point)
				    (+ 1 (point) suffix-replace-chars))))))
      (move-overlay ov (point) (+ 1 (point) suffix-replace-chars))
      (if (eobp)
	  (progn
	    (overlay-put ov 'display "")
	    (overlay-put ov 'after-string p-completion))
 	(overlay-put ov 'display (substring p-completion 0 1))
	(overlay-put ov 'after-string (substring p-completion 1)))
      (overlay-put ov 'completion completion))))

(defun tabby--overlay-show-completion (request response)
  "Render overlay."
  (tabby--clear-overlay)
  (when-let* ((choices (plist-get (cadr response) :choices))
	      (choice (car choices))
	      (choice-text (plist-get choice :text))
	      ((not (zerop (length choice-text))))
	      (replace-range (plist-get choice :replaceRange))
	      (start (plist-get replace-range :start))
	      (end (plist-get replace-range :end))
	      (pos (plist-get request :position))
	      (prefix-replace-chars (- pos start))
	      (suffix-replace-chars (- end pos))
	      (text (substring choice-text prefix-replace-chars))
	      ((not (zerop (length text))))
	      (ov (tabby--get-overlay)))
    (when (= (point) (+ 1 tabby--position-offset pos))
      (overlay-put ov 'replace-end (+ 1 tabby--position-offset end))
      (overlay-put ov 'suffix-replace-chars suffix-replace-chars)
      (overlay-put ov 'completion-id (plist-get (cadr response) :id))
      (overlay-put ov 'choice-index (plist-get choice :index))
      (tabby--set-overlay-text ov text))))

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
(defvar tabby--post-command-timer nil)

(defcustom tabby-idle-delay 0
  "Time in seconds to wait before starting completion.

Complete immediately if set to 0.
Disable idle completion if set to nil."
  :type '(choice
          (number :tag "Seconds of delay")
          (const :tag "Idle completion disabled" nil))
  :group 'tabby)

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

(defun tabby--overlay-end (ov)
  "Return the end position of overlay OV."
  (- (line-end-position) (overlay-get ov 'tail-length)))

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

(defun tabby--satisfy-display-predicates ()
  "Return t if all display predicates are satisfied."
  (tabby--satisfy-predicates tabby-enable-display-predicates tabby-disable-display-predicates))

(defun tabby--mode-enter ()
  "Set up tabby mode when entering."
  (add-hook 'post-command-hook 'tabby--post-command nil 'local))

(defun tabby--mode-exit ()
  "Clean up tabby mode when exiting."
  (remove-hook 'post-command-hook #'tabby--post-command 'local))

;;;###autoload
(define-global-minor-mode global-tabby-mode
  tabby-mode tabby-turn-on-unless-buffer-read-only)

(defun tabby-turn-on-unless-buffer-read-only ()
  "Turn on `tabby-mode' if the buffer is writable."
  (unless buffer-read-only
    (tabby-mode 1)))

(provide 'tabby)
;;; tabby.el ends here
