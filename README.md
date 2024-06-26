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

Or using straight.el:

```lisp
(use-package tabby
  :straight (tabby
	     :type git
	     :host github
	     :files ("*.el" "node_scripts")
	     :repo "alan-w-255/tabby.el"))
```

!!important!!
Follow the [document](https://tabby.tabbyml.com/docs/extensions/configurations) to config the tabby client.

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

## Customization

- `tabby-enable-predicates` A list of predicate functions with no argument to enable Tabby.
Tabby will be triggered only if all predicates return t.

If you are using evil, you may be recommended to add `evil-insert-state-p` to `tabby-enable-predicates`.

Example:

```emacs-lisp
(setq tabby-enable-predicates '(evil-insert-state-p))
```

- `tabby-disable-predicates` A list of predicate functions with no argument to disable Tabby.
Tabby will not be triggered if any predicate returns t.

- `tabby-idle-delay` Time in seconds to wait before starting completion. Complete immediately if set to 0. Disable idle completion if set to nil.

- `tabby-enable-display-predicates` A list of predicate functions with no argument to enable Tabby.
Tabby will show completions only if all predicates return t.

- `tabby-disable-display-predicates` A list of predicate functions with no argument to disable Tabby.
Tabby will not show completions if any predicate returns t.

## Note

This plugin works on my machine, but there may be many bugs that have not been discovered yet. Please feel free to raise an issue if you encounter any problems.

The node_script folder is copied from https://github.com/TabbyML/tabby/tree/main/clients/vim/node_scripts . I don't know much about javascript. It just works. You can also copy the node_scripts folder from the official tabby repository.

## Acknowledgements

- [copilot.el](https://github.com/copilot-emacs/copilot.el)
- [tabby](https://github.com/TabbyML/tabby)
