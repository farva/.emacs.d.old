(defun ad-mkvirtualenv (&rest args)
  (interactive)
  (venv--check-executable)
  (let ((parent-dir (if (stringp venv-location)
                        (file-name-as-directory
                         (expand-file-name venv-location))
                      default-directory))
        (python-exe-arg (when current-prefix-arg
                          (concat "--python="
                                  (read-string "Python executable: " "python"))))
        (name (if args (car (last args))
                (read-from-minibuffer "New virtualenv: "))))
    ;; error if this env already exists
    (when (-contains? (venv-get-candidates) name)
      (error "A virtualenv with this name already exists!"))
    (run-hooks 'venv-premkvirtualenv-hook)
    (shell-command (concat "virtualenv "
                           python-exe-arg " "
                           (mapconcat 'identity (butlast args) " ") " "
                           parent-dir
                           name))
    (when (listp venv-location)
      (add-to-list 'venv-location (concat parent-dir it)))
    (venv-with-virtualenv name
                          (run-hooks 'venv-postmkvirtualenv-hook))
    (when (called-interactively-p 'interactive)
      (message (concat "Created virtualenv: " name)))))

(defun venv-mkvirtualenv-inherit (name)
  (interactive "sName: ")
  (venv-mkvirtualenv "--system-site-packages" name))

(when (getenv "WORKON_HOME")
  (require 'virtualenvwrapper)
  (venv-initialize-interactive-shells) ;; if you want interactive shell support
  (venv-initialize-eshell) ;; if you want eshell support
  (setq venv-location (getenv "WORKON_HOME"))
  (advice-add 'venv-mkvirtualenv :override #'ad-mkvirtualenv))

(provide 'init-virtualenvwrapper)
