;;; pff.el ---                                       -*- lexical-binding: t; -*-

;; Copyright (C) 2018  onohiroko

;; Author: onohiroko <kuanyui.github.io>
;; License: WTFPL 2.0

;;; Code:

(defvar pff-candidates-limit 200)
(defvar pff-recents-enabled t)
(defvar pff-recents-limit 50)
(defvar pff-recents (make-hash-table :test 'equal))

(defun pff-add-recent-file (path)
  (let* ((old-file-list (gethash (pff-pwd) pff-recents))
         (new-file-list (progn
                          (if (> (length old-file-list) pff-recents-limit)
                              (setq old-file-list (butlast old-file-list)))
                          (cons path (remove path old-file-list)))))
    (puthash (pff-pwd) new-file-list pff-recents)))

(defun pff-get-recent-files ()
  (gethash (pff-pwd) pff-recents))

(defun pff-call-process-to-string-list (program &rest args)
  "`shell-command-to-string' is too slow for simple task, so use this."
  (with-temp-buffer
    (apply #'call-process program (append '(nil t nil) args))
    (split-string (buffer-string) "\n")))

(defun pff-get-candidates-list (str)
  (let ((default-directory (pff-pwd)))
    (if pff-recents-enabled
        (let ((recent-file-list (pff-get-recent-files)))
          (append recent-file-list
                  (remove-if (lambda (path) (member path recent-file-list))
                             (if (> (length str) 0)
                                 (pff-call-process-to-string-list "ag" "-g" (shell-quote-argument str))))))
      (pff-call-process-to-string-list "ag" "-g" (shell-quote-argument str)))))

(defun pff-pwd ()
  (or (locate-dominating-file "." ".git") default-directory))

(defun pff-find-file (relative-path)
  (let ((abs-path (concat (pff-pwd) relative-path)))
    (if pff-recents-enabled (pff-add-recent-file relative-path))
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
