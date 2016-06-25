(require 'init-helm)

(my:install-package-if-needed 'helm-gtags)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)
(add-hook 'python-mode-hook 'helm-gtags-mode)

(defun bhj-isearch-from-bod (&optional col-indent)
  (interactive "p")
  (let ((word (current-word))
        (org-point (point))
        new-point)
    (save-excursion
      (beginning-of-defun)
      ;; (setq regexp-search-ring (cons (concat "\\b" word "\\b") regexp-search-ring))
      (setq new-point (re-search-forward (concat "\\_<" word "\\_>") org-point t)))
    (when new-point
      (push-mark-no-activate)
      (goto-char new-point))))

(eval-after-load 'helm-gtags
  '(progn
     (define-key helm-gtags-mode-map (concat helm-gtags-prefix-key (kbd "a"))
       'helm-gtags-tags-in-this-function)
     (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
     (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "C-c v") 'bhj-isearch-from-bod)))

(defvar-local my:helm-gtags-follow-links t)
(defun my:helm-gtags--real-file-name ()
  (let ((buffile (buffer-file-name)))
    (unless buffile
      (error "This buffer is not related to file."))
    (if (file-remote-p buffile)
        (tramp-file-name-localname (tramp-dissect-file-name buffile))
      (if my:helm-gtags-follow-links
          (file-truename buffile)
        (expand-file-name buffile)))))
(advice-add 'helm-gtags--real-file-name
            :override #'my:helm-gtags--real-file-name)

(with-eval-after-load 'elpy
  (define-key elpy-mode-map (kbd "C-<") nil)
  (define-key elpy-mode-map (kbd "C->") nil)
  (define-key elpy-mode-map (kbd "M-.") nil))

(provide 'init-helm-gtags)
