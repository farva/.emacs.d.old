(my:install-package-if-needed 'projectile)

(projectile-global-mode)

(my:install-package-if-needed 'helm-projectile)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; (persp-mode)
;;(require 'persp-projectile)

(provide 'init-projectile)
