;; irony-mode
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(add-hook 'irony-mode-hook 'my-irony-mode-hook)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(defun my-ac-irony-hook ()
  ;; be cautious, if yas is not enabled before (auto-complete-mode 1), overlays
  ;; *may* persist after an expansion.
  ;(yas-minor-mode 1)
  ;(auto-complete-mode 1)

  (add-to-list 'ac-sources 'ac-source-irony)
  (define-key irony-mode-map (kbd "M-RET") 'ac-complete-irony-async))

(eval-after-load 'auto-complete
  '(progn
     ;; add to auto-complete
     (require 'ac-company)
     (ac-company-define-source ac-source-company-irony company-irony)

     (let ((ac-irony-dir (concat user-emacs-directory (convert-standard-filename "repos/ac-irony"))))
       (when (file-exists-p ac-irony-dir)
         (add-to-list 'load-path ac-irony-dir)
         (require 'ac-irony)
         (add-hook 'irony-mode-hook 'my-ac-irony-hook)))))

(provide 'init-irony)
