;; store all backup and autosave files in the tmp dir
(let ((my-temp-dir (concat temporary-file-directory
                           (convert-standard-filename "emacs/"))))
  (setq backup-directory-alist
        `(("." . ,my-temp-dir)))
  (setq auto-save-file-name-transforms
        `((".*" ,my-temp-dir t))))

(provide 'init-temp-files)
