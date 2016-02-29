;; enable CUA just for what I want
(cua-selection-mode t)

;;(setq cua-enable-cua-keys nil)
;;(setq cua-highlight-region-shift-only t) ;; no transient mark mode
;;(setq cua-toggle-set-mark nil) ;; original set-mark behavior, i.e. no transient-mark-mode
;;(cua-mode)

(define-key cua-global-keymap (kbd "C-<return>") nil)
(define-key cua-global-keymap (kbd "C-c C-SPC") 'cua-set-rectangle-mark)

(provide 'init-cua)
