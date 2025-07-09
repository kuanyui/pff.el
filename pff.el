;;; pff.el ---                                       -*- lexical-binding: t; -*-

;; Copyright (C) 2018  onohiroko

;; Author: onohiroko <kuanyui.github.io>
;; License: WTFPL 2.0

;;; Code:

(defvar pff-candidates-limit 200)
(defvar pff-recents-enable t)
(defvar pff-recents-limit 50)
(defvar pff-opened-buffers-first t)

(defvar pff-recents (make-hash-table :test 'equal))
;; (defvar-local pff--current-filepath "")
(defun pff-get-buffer-files ()
  "Get opened buffers which are under the root of current project."
  (remove-if (lambda (file-path) (not (string-prefix-p (pff-project-root) file-path)))
             (remove nil (mapcar #'buffer-file-name (buffer-list)))))

(defun pff-project-root ()
  (let ((found (locate-dominating-file "." ".git")))
    (if found
        (expand-file-name found)
      nil)))

(defun pff-add-recent-file (path)
  (let* ((old-file-list (gethash (pff-project-root) pff-recents))
         (new-file-list (progn
                          (if (> (length old-file-list) pff-recents-limit)
                              (setq old-file-list (butlast old-file-list)))
                          (cons path (remove path old-file-list)))))
    (puthash (pff-project-root) new-file-list pff-recents)))

(defun pff-get-recent-files ()
  (let* ((default-directory (pff-project-root))
         (recent-files (gethash default-directory pff-recents))
         (valid-recent-files (remove-if-not #'file-exists-p recent-files)))
    (puthash default-directory valid-recent-files pff-recents)
    valid-recent-files))

(defun pff-call-process-to-string-list (program &rest args)
  "`shell-command-to-string' is too slow for simple task, so use this."
  (with-temp-buffer
    (apply #'call-process program (append '(nil t nil) args))
    (cl-remove-if #'string-empty-p (split-string (buffer-string) "\n"))))

(defun pff-execute-backend-to-get-file-list (pattern)
  (cond
   ;;((executable-find "rg")
   ;;(pff-call-process-to-string-list "rg" "--hidden" "--files" "--glob" (shell-quote-wildcard-pattern pattern)))
   ((executable-find "ag")
    (pff-call-process-to-string-list "ag" "--hidden" "--filename-pattern" (shell-quote-argument pattern)))
   ((executable-find "find")
    (pff-call-process-to-string-list "find" (shell-quote-argument (pff-project-root)) "-type" "f" "-name" (shell-quote-wildcard-pattern (format "*%s*" pattern))))))

(defun pff-get-candidates-list (str &optional recent-file-list)
  (let ((default-directory (pff-project-root)))
    (if pff-recents-enable
	(append recent-file-list
		(remove-if (lambda (path) (member path recent-file-list))
                           (if (> (length str) 0)
                               (pff-execute-backend-to-get-file-list str))))
      (pff-execute-backend-to-get-file-list str))))

(defun pff-find-file (fpath)
  (let ((abs-path (if (file-name-absolute-p fpath)
		      fpath
		    (concat (pff-project-root) fpath))))
    (if pff-recents-enable (pff-add-recent-file fpath))
    (find-file abs-path)))



;;;###autoload
(defun pff ()
  (interactive)
  (if (pff-project-root)
      (let ((recent-files (pff-get-recent-files)))
        (helm :sources (helm-build-sync-source "pff"
                         :candidates (lambda () (pff-get-candidates-list helm-pattern recent-files))
                         :volatile t
                         :action #'pff-find-file
                         :candidate-number-limit pff-candidates-limit
                         :header-name (lambda (_) (format "Project: %s"
                                                          (file-name-base (directory-file-name (pff-project-root)))))
                         )
              :buffer "*pff*"
              :prompt "File name: "))
    (message "Not in a project. Abort.")))

(provide 'pff)
;;; pff.el ends here
