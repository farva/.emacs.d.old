(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-auto-revert-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "unknown" :slant normal :weight normal :height 151 :width normal)))))

(let ((default-directory
	(concat user-emacs-directory
		(convert-standard-filename "elisp/"))))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'remote-shell)

;; initialize remote-shell
(setq remote-shell-resources-path
      (concat user-emacs-directory (convert-standard-filename "elisp/")))

;; indentation
(setq-default c-basic-offset 2)
(setq-default indent-tabs-mode nil)
