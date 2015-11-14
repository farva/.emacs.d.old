(use-package ycmd
  :ensure t)

(add-hook 'after-init-hook #'global-ycmd-mode)
(set-variable 'ycmd-server-command `("python2.7"
                                     ,(expand-file-name
                                       (concat user-emacs-directory
                                              (convert-standard-filename
                                               "repos/ycmd/ycmd")))))

(eval-after-load 'company
  '(progn
     (use-package company-ycmd
       :ensure t)
     (company-ycmd-setup)))

(provide 'init-ycmd)
