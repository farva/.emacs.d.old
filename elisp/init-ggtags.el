(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

;; (setq ggtags-auto-jump-to-match nil)

;; (eval-after-load "ggtags"
;;   '(progn
;;      (define-key ggtags-mode-map (kbd "C-M-.") nil)
;;      (define-key ggtags-mode-map (kbd "C-x M-.") 'ggtags-find-tag-regexp)
;;      (define-key ggtags-mode-map (kbd "M-.") (function tags-find-symbol-at-point))
;;      (define-key ggtags-mode-map (kbd "M-,") (function tags-find-references-at-point))))

(provide 'init-ggtags)
