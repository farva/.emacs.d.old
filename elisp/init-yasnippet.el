(use-package yasnippet
  :ensure t)

(yas-global-mode 1)

(defun yas-no-expand-in-comment/string ()
  (setq yas-buffer-local-condition
        '(if (nth 8 (syntax-ppss)) ;; non-nil if in a string or comment
             '(require-snippet-condition . force-in-comment)
           t)))
(add-hook 'prog-mode-hook 'yas-no-expand-in-comment/string)

(defun yas-expand-or-space ()
  "Either expand or write <SPC>."
  (interactive)
  (let ((yas-fallback-behavior '(apply insert-char . (? )))
        ;; (yas--inhibit-overlay-hooks
        ;;  (cons 'indent-according-to-mode yas--inhibit-overlay-hooks))
        )
    (yas-expand)))

(defun yas-space-or-maybe-expand-in-field ()
  "Either expand or write <SPC> inside field"
  (interactive)
  (if yas-triggers-in-field
      (let ((yas-fallback-behavior 'return-nil)
            (active-field (overlay-get yas--active-field-overlay 'yas--field)))
        (when active-field
          (unless (yas-expand-from-trigger-key active-field)
            (insert-char ? ))))
    (insert-char ? )))

;; (eval-after-load 'cc-mode
;;   '(progn
;;      (define-key c-mode-map (kbd "<SPC>") 'yas-expand-or-space)
;;      (define-key c++-mode-map (kbd "<SPC>") 'yas-expand-or-space)))

(define-key yas-minor-mode-map (kbd "<SPC>") 'yas-expand-or-space)
(define-key yas-keymap (kbd "<SPC>") 'yas-space-or-maybe-expand-in-field)

(defvar-local yas-snippet-scope-initial-end-pos nil
  "Scope's end position.")
(defvar-local yas-snippet-scope-current-end-pos nil
  "Current position of scope's end.")
(defvar-local yas-snippet-scope-start-extend-hook '()
  "Hooks to run when starting to extend a scope beyond intial position.")
(defvar-local yas-snippet-scope-end-contract-hook '()
  "Hooks to run when a scope is contracted back to its original size.")
(defvar-local yas-snippet-scope-blob nil
  "Blob to be used by the embeded code of the snippet.")

(defun yas-snippet-scope-init-pos (&optional pos)
  "Init `yas-snippet-scope-initial-end-pos' and `yas-snippet-scope-current-end-pos' to point or `pos'."
  (let ((end-pos (if pos pos (point))))
    (setq yas-snippet-scope-initial-end-pos end-pos)
    (setq yas-snippet-scope-current-end-pos end-pos)
    nil))

(defun yas-snippet-scope-clear-values ()
  "Clear values of `yas-snippet-scope-initial-end-pos' and `yas-snippet-scope-current-end-pos'"
  (setq yas-snippet-scope-initial-end-pos nil)
  (setq yas-snippet-scope-current-end-pos nil)
  (setq yas-snippet-scope-start-extend-hook '())
  (setq yas-snippet-scope-end-contract-hook '())
  (setq yas-snippet-scope-blob nil))

(add-hook 'yas-before-expand-snippet-hook 'yas-snippet-scope-clear-values)
(add-hook 'yas-after-exit-snippet-hook 'yas-snippet-scope-clear-values)

(defun yas-snippet-scope-extend ()
  "Extend the scope of the snippet downwards."
  (interactive)
  (if (and (yas--snippets-at-point)
           yas-snippet-scope-initial-end-pos)
      (save-excursion
        (goto-char yas-snippet-scope-current-end-pos)
        (call-interactively 'drag-stuff-down)
        (when (and (= yas-snippet-scope-current-end-pos
                      yas-snippet-scope-initial-end-pos)
                   (> (point) yas-snippet-scope-current-end-pos))
          (run-hooks 'yas-snippet-scope-start-extend-hook))
        (indent-region yas-snippet-scope-current-end-pos (point))
        (setq yas-snippet-scope-current-end-pos (point)))
    (error "No snippet scope to extend.")))

(defun yas-snippet-scope-contract ()
  "Contract the scope of the snippet upwards."
  (interactive)
  (if (and (yas--snippets-at-point)
           yas-snippet-scope-initial-end-pos)
      (if (> yas-snippet-scope-current-end-pos
             yas-snippet-scope-initial-end-pos)
          (save-excursion
        (goto-char yas-snippet-scope-current-end-pos)
        (call-interactively 'drag-stuff-up)
        (when (= (point) yas-snippet-scope-initial-end-pos)
          (run-hooks 'yas-snippet-scope-end-contract-hook))
        (indent-region (point) yas-snippet-scope-current-end-pos)
        (setq yas-snippet-scope-current-end-pos (point)))
        (error "Cannot contract snippet's scope any further."))
    (error "No snippet scope to contract.")))

;; (define-key yas-minor-mode-map (kbd "C-M-<next>") 'yas-snippet-scope-extend)
;; (define-key yas-minor-mode-map (kbd "C-M-<prior>") 'yas-snippet-scope-contract)

(setq yas-triggers-in-field t)

;; (drag-stuff-global-mode)

(provide 'init-yasnippet)
