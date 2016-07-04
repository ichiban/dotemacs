(add-to-list 'load-path "~/.emacs.d/lisp")

;; \C-h to backspace
(global-set-key "\C-h" 'backward-delete-char)

;; load utils
(load "ichiban.el")

;; Dynamic Macro
(defconst *dmacro-key* "\C-t" "繰返し指定キー")
(global-set-key *dmacro-key* 'dmacro-exec)
(autoload 'dmacro-exec "dmacro" nil t)

;; hide tool bar
(tool-bar-mode -1)

;; hide scroll bar
(scroll-bar-mode -1)

;; never blink cursor
(blink-cursor-mode -1)

;; never show startup message
(setq inhibit-startup-message t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; color-theme
(require 'color-theme)
(color-theme-initialize)
(require 'zenburn-theme)

;; css
(setq css-indent-offset 2)

;; js
(setq js-indent-level 2)

;; rust
(require 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; common lisp
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; OCaml
(load "/Users/ichiban/.opam/system/share/emacs/site-lisp/tuareg-site-file")
(add-to-list 'load-path "/Users/ichiban/.opam/system/share/emacs/site-lisp/")
;; Add opam emacs directory to the load-path
(setq opam-share
      (substring
       (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
;; Load merlin-mode
(require 'merlin)
;; Start merlin on ocaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)
;; Enable auto-complete
(setq merlin-use-auto-complete-mode 'easy)
;; Use opam switch to lookup ocamlmerlin binary
(setq merlin-command 'opam)
;; indentations
(setq tuareg-indent-align-with-first-arg nil)
(put 'upcase-region 'disabled nil)


;; Golang
(require 'go-mode)
;; go-errcheck
(require 'go-errcheck)
;; gocode
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)
;; oracle
(defun my-go-mode-hook ()
  (load-file "/ssh:relax:$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el"))
(add-hook 'go-mode-hook 'my-go-mode-hook)
