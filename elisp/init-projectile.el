(my:install-package-if-needed 'projectile)

(projectile-global-mode)

(setq projectile-switch-project-action 'projectile-dired)

(my:install-package-if-needed 'helm-projectile)
(with-eval-after-load 'helm
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

;; (persp-mode)
;;(require 'persp-projectile)

(provide 'init-projectile)
