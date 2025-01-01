;;; ichiban.el --- my own utilities
;;; Commentary:

;; Copyright (C) 2010  ICHIBANGASE, Yutaka

;; Author: ICHIBANGASE, Yutaka <yichiban@gmail.com>
;; Keywords: utils

;;; Code:

(let ((n 0))
  (defun tmp ()
    "一時バッファを生成する."
    (interactive)
    (switch-to-buffer
     (get-buffer-create (concat "tmp" (int-to-string n))))
    (setq n (+ n 1))))

(defun flow ()
  "Toggle full-screen."
  (interactive)
  (if (fboundp 'ns-toggle-fullscreen)
      (ns-toggle-fullscreen); carbon emacs has this feature
    (if (frame-parameter nil 'fullscreen)
	(set-frame-parameter nil 'fullscreen nil)
      (set-frame-parameter nil 'fullscreen 'fullboth))))
  
(global-set-key [f11] 'flow)
(add-hook 'after-make-frame-functions 'flow)

(provide 'ichiban)
;;; ichiban.el ends here
