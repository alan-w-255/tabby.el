# tabby.el

An unofficial [tabby](https://github.com/TabbyML/tabby) plugin for Emacs. Heavily borrow from [copilot.el](https://github.com/copilot-emacs/copilot.el) and the official tabby.vim plugin.

## Install

Ensure you installed Node.js v18+. (You can specify the path to node executable by setting tabby-node-executable.)

git clone this repository.

```lisp
(add-to-list 'load-path "path_to_tabby.el_directory")
(require 'tabby)

;; example. active tabby-mode locally when entering go-ts-mode.
(add-hook 'go-ts-mode-hook 'tabby-mode)
```

## Usage

Here is my configuration.

```lisp
(evil-define-key 'insert tabby-mode-map
  (kbd "C-j") 'tabby-accept-completion)

(evil-define-key 'insert tabby-mode-map
  (kbd "C-<tab>") 'tabby-accept-completion-by-word)

(evil-define-key 'insert tabby-mode-map
  (kbd "C-l") 'tabby-accept-completion-by-line)
```

## warning

This plugin works on my machine, but there may be many bugs that have not been discovered yet. Please feel free to raise an issue if you encounter any problems.

