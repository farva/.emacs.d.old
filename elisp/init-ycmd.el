(require 'ycmd)
(add-hook 'after-init-hook #'global-ycmd-mode)
(set-variable 'ycmd-server-command '("python2.7" "/home/oren/workspace/ycmd/ycmd"))

(eval-after-load 'company
  '(progn
     (require 'company-ycmd)
     (company-ycmd-setup)))

(provide 'init-ycmd)
