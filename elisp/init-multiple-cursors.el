(my:use-package-ensure multiple-cursors)
(my:use-package-ensure region-bindings-mode)

(define-key region-bindings-mode-map (kbd "C-s-c") 'mc/edit-lines)
(define-key region-bindings-mode-map (kbd "C-]") 'mc/mark-next-like-this)
(define-key region-bindings-mode-map (kbd "C-[") 'mc/mark-previous-like-this)
(define-key region-bindings-mode-map (kbd "C-c C-]") 'mc/mark-all-like-this)

(global-set-key (kbd "C-c C-SPC") 'set-rectangular-region-anchor)

(provide 'init-multiple-cursors)
