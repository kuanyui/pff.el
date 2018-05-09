;;; pff.el ---                                       -*- lexical-binding: t; -*-

;; Copyright (C) 2018  onohiroko

;; Author: onohiroko <kuanyui.github.io>
;; License: WTFPL 2.0

;;; Code:

(setq pff-limit 50)

(defun pff-call-process-to-string (program &rest args)
  "`shell-command-to-string' is too slow for simple task, so use this."
  (with-temp-buffer
    (apply #'call-process program (append '(nil t nil) args))
    (split-string (buffer-string) "\n")))

(defun pff-get-candidates-list (str)
  (let ((default-directory (pff-pwd)))
    (pff-call-process-to-string "ag" "-g" (shell-quote-argument str))))

(defun pff-pwd ()
  (or (locate-dominating-file "." ".git") default-directory))

(defun pff-find-file (relative-path)
  (find-file (concat (pff-pwd) relative-path)))

(defun pff ()
  (interactive)
  (helm :sources (helm-build-sync-source "pff-sync"
                   :candidates (lambda () (pff-get-candidates-list helm-pattern))
                   :volatile t
                   :action #'pff-find-file
                   :candidate-number-limit pff-limit
                   :requires-pattern t)
        :buffer "*pff*"
        :prompt "File name: "))

(provide 'pff)
;;; pff.el ends here
