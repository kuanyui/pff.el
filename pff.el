;;; pff.el ---                                       -*- lexical-binding: t; -*-

;; Copyright (C) 2018  onohiroko

;; Author: onohiroko <kuanyui.github.io>
;; License: WTFPL 2.0

;;; Code:

(defvar pff-candidates-limit 200)
(defvar pff-recents-enabled t)
(defvar pff-recents-limit 50)
(defvar pff-recents nil)

(defun pff-add-recents (path)
  (setq pff-recents (cons path (remove path pff-recents))))

(defun pff-call-process-to-string-list (program &rest args)
  "`shell-command-to-string' is too slow for simple task, so use this."
  (with-temp-buffer
    (apply #'call-process program (append '(nil t nil) args))
    (split-string (buffer-string) "\n")))

(defun pff-get-candidates-list (str)
  (let ((default-directory (pff-pwd)))
    (if pff-recents-enabled
        (append pff-recents
                (remove-if (lambda (path) (member path pff-recents))
                           (if (> (length str) 0)
                               (pff-call-process-to-string-list "ag" "-g" (shell-quote-argument str)))))
      (pff-call-process-to-string-list "ag" "-g" (shell-quote-argument str)))))

(defun pff-pwd ()
  (or (locate-dominating-file "." ".git") default-directory))

(defun pff-find-file (relative-path)
  (let ((abs-path (concat (pff-pwd) relative-path)))
    (if pff-recents-enabled (pff-add-recents relative-path))
    (find-file abs-path)))

(defun pff ()
  (interactive)
  (helm :sources (helm-build-sync-source "pff-sync"
                   :candidates (lambda () (pff-get-candidates-list helm-pattern))
                   :volatile t
                   :action #'pff-find-file
                   :candidate-number-limit pff-candidates-limit
                   )
        :buffer "*pff*"
        :prompt "File name: "))

(provide 'pff)
;;; pff.el ends here
