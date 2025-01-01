;;; init.el --- initialization
;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/lisp")

;; \C-h to backspace
(global-set-key "\C-h" 'backward-delete-char)

;; load utils
(load "ichiban.el")

;; Dynamic Macro
(defconst *dmacro-key* "\C-t" "繰返し指定キー.")
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

(dolist (package '(auto-complete
		   flycheck
		   rust-mode
		   merlin
		   go-mode
		   go-errcheck
		   go-autocomplete
		   go-eldoc
		   gotest
		   ellama
		   zenburn-theme))
  (unless (package-installed-p package)
    (package-install package)))

(load-theme 'zenburn t)

(provide 'init)
;;; init.el ends here
