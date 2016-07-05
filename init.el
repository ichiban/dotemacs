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
(unless package-archive-contents
  (package-refresh-contents))

(dolist (package '(color-theme
		   zenburn-theme
		   auto-complete
		   direx
		   flycheck
		   rust-mode
		   merlin
		   go-mode
		   go-errcheck
		   go-autocomplete
		   go-eldoc
		   go-direx))
  (unless (package-installed-p package)
    (package-install package)))

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
(setq slime-helper-file (expand-file-name "~/quicklisp/slime-helper.el"))
(when (file-exists-p slime-helper-file)
    (load slime-helper-file))
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; OCaml
(setq tuareg-site-file
      "~/.opam/system/share/emacs/site-lisp/tuareg-site-file")
(when (file-exists-p tuareg-site-file)
  (load tuareg-site-file))
(when (file-exists-p "~/.opam")
  (add-to-list 'load-path "~/.opam/system/share/emacs/site-lisp/")
  ;; Add opam emacs directory to the load-path
  (setq opam-share
	(substring
	 (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
  (add-to-list 'load-path (concat opam-share "/emacs/site-lisp")))
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
;; go-eldoc
(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)
;; go-direx
(require 'go-direx)
(define-key go-mode-map (kbd "C-c C-j") 'go-direx-pop-to-buffer)
;; go-guru
(defvar go-guru-file
  (substitute-in-file-name
   "$GOPATH/src/golang.org/x/tools/cmd/guru/go-guru.el"))
(when (file-exists-p go-guru-file)
  (load go-guru-file)
  (add-hook 'go-mode-hook 'go-guru-mode))

;; Flycheck
(require 'flycheck)
(global-flycheck-mode)
