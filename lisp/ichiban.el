;;; ichiban.el --- my own utilities

;; Copyright (C) 2010  ICHIBANGASE, Yutaka

;; Author: ICHIBANGASE, Yutaka <yichiban@gmail.com>
;; Keywords: utils

(require 'cl)

(defmacro* narrow-if-needed (&body body)
  "transient-mark-modeだった場合はbodyのオペレーションがリージョン内に制限される"
  (lexical-let ((needed (gensym)))
    `(lexical-let ((,needed (and transient-mark-mode mark-active))
		   result)
       (when ,needed (narrow-to-region (region-beginning) (region-end)))
       (setq result (progn ,@body))
       (when ,needed (widen))
       result)))

(defun* mark-whole-buffer-if-needed ()
  "transient-mark-modeでない場合はバッファ全体がマークされます"
  (lexical-let ((needed (not (and transient-mark-mode mark-active))))
    (if needed (mark-whole-buffer))))

(lexical-let ((n 0))
  (defun* tmp ()
    "一時バッファを生成する"
    (interactive)
    (switch-to-buffer
     (get-buffer-create (concat "tmp" (int-to-string n))))
    (incf n)))

(defun* uniq ()
  "uniqコマンドに相当する。transient-mark-modeだった場合はリージョンに対してのみ作用する"
  (interactive)
  (narrow-if-needed
   (beginning-of-buffer)
   (while (re-search-forward "\\(.*\n\\)\\1+" nil t)
     (replace-match "\\1" nil nil))))

(defun* mecab (&optional (option "-Owakati"))
  "mecabを用いて形態素解析する。transient-mark-modeだった場合はリージョンに対してのみ作用する"
  (interactive)
  (mark-whole-buffer-if-needed)
  (lexical-let ((b (region-beginning))
		(e (region-end)))
    (call-process-region b e "mecab" t t nil option))
  (delete-backward-char 2))

(defun* growl (msg)
  "growlnotifyでGrowlにメッセージを表示する"
  (interactive "sMessage:")
  (shell-command
   (format "growlnotify -m \"%s\"" msg)))

(defun* hankaku ()
  "携帯用に対応する半角文字があれば置換する"
  (interactive)
  (narrow-if-needed
   (beginning-of-buffer)
   (while (re-search-forward "\[ァ-ヶー、。？！―・　（）／「」“”０-９Ａ-Ｚ\]+" nil t)
     (japanese-hankaku-region (match-beginning 0) (match-end 0)))))

(defun* censored ()
  "検閲文字列を挿入"
  (interactive)
  (insert "<censored>"))

(defun* tweet (status)
  (twittering-call-api 'update-status `((status . ,status))))

(defun* rand-8-digits ()
  ""
  (interactive)
  (insert (number-to-string (random 10)))
  (insert (number-to-string (random 10)))
  (insert (number-to-string (random 10)))
  (insert (number-to-string (random 10)))
  (insert (number-to-string (random 10)))
  (insert (number-to-string (random 10)))
  (insert (number-to-string (random 10)))
  (insert (number-to-string (random 10))))

(defun* jsonpp (&optional b e)
  "JSON Pretty Pringing"
  (interactive "r")
  (shell-command-on-region b e "jsonpp"))

(defun base64 (&optional b e)
  "base64 encode/decode"
  (interactive "r")
  (shell-command-on-region b e "ruby -rbase64 -e 's = $stdin.gets; print Base64.send(/^[A-Za-z0-9\\+\\/=]+$/ =~ s ? :decode64 : :encode64, s).split($/).join'" nil t))

(defun tsv-to-sexp (&optional b e)
  "TSV to S-exp"
  (interactive "r")
  (lexical-let ((str (buffer-substring b e)))
    (delete-region b e)
    (insert (pprint (mapcar (lambda (line) (split-string line "\t"))
			    (split-string str "\n"))))))

(defun pprint (form &optional output-stream)
  "pretty print S-exp found at http://stackoverflow.com/a/3552347"
  (princ (with-temp-buffer
           (cl-prettyprint form)
           (buffer-string))
         output-stream))

(defun tsv-to-ruby (&optional b e)
  "TSV to ruby"
  (interactive "r")
  (lexical-let ((str (buffer-substring b e)))
    (delete-region b e)
    (insert (mapconcat #'identity
		       (list "["
			     (mapconcat #'tsv-to-ruby-line (split-string str "\n") ",\n ")
			     "]")
		       ""))))

(defun tsv-to-ruby-line (line)
  (mapconcat #'identity
	     (list "["
		   (mapconcat #'tsv-to-ruby-elem (split-string line "\t") ", ")
		   "]")
	     ""))

(defun tsv-to-ruby-elem (elem)
  (format "%S" elem))

(defun flow ()
  "toggle full-screen"
  (interactive)
  (if (fboundp 'ns-toggle-fullscreen)
      (ns-toggle-fullscreen); carbon emacs has this feature
    (if (frame-parameter nil 'fullscreen)
	(set-frame-parameter nil 'fullscreen nil)
      (set-frame-parameter nil 'fullscreen 'fullboth))))
  
(global-set-key [f11] 'flow)
(add-hook 'after-make-frame-functions 'flow)
