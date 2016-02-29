(my:install-package-if-needed 'projectile)

(projectile-global-mode)

(eval-after-load 'init-helm
  '(progn
     (my:install-package-if-needed 'helm-projectile)
     (setq projectile-completion-system 'helm)
     (helm-projectile-on)))

;; (persp-mode)
;;(require 'persp-projectile)

(provide 'init-projectile)
