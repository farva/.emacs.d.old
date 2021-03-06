(setq rtags-src-dir (concat user-emacs-directory (convert-standard-filename "repos/rtags/src/")))
(load (concat rtags-src-dir "rtags"))
(load (concat rtags-src-dir "rtags-helm"))
(load (concat rtags-src-dir "company-rtags"))

(require 'rtags-helm)
(setq rtags-use-helm t)

(setq rtags-autostart-diagnostics t)
(setq rtags-completions-enabled t)
(with-eval-after-load 'company
  (push 'company-rtags company-backends))

(defun use-rtags (&optional useFileManager)
  (and (rtags-executable-find "rc")
       (cond ((not (helm-gtags--tag-directory)) t)
             ((and (not (eq major-mode 'c++-mode))
                   (not (eq major-mode 'c-mode))) (rtags-has-filemanager))
             (useFileManager (rtags-has-filemanager))
             (t (rtags-is-indexed)))))

(with-eval-after-load 'init-helm-gtags
  (defun tags-find-symbol-at-point (&optional prefix)
    (interactive "P")
    (if (or (not (rtags-find-symbol-at-point prefix)) rtags-last-request-not-indexed)
        (helm-gtags-dwim)))
  (defun tags-find-references-at-point (&optional prefix)
    (interactive "P")
    (if (or (not (rtags-find-references-at-point prefix)) rtags-last-request-not-indexed)
        (helm-gtags-dwim)))
  (defun tags-find-symbol ()
    (interactive)
    (call-interactively (if (use-rtags) 'rtags-find-symbol 'helm-gtags-find-symbol)))
  (defun tags-find-references ()
    (interactive)
    (call-interactively (if (use-rtags) 'rtags-find-references 'helm-gtags-find-rtag)))
  (defun tags-find-file ()
    (interactive)
    (call-interactively (if (use-rtags t) 'rtags-find-file 'helm-gtags-find-files)))
  (defun tags-imenu ()
    (interactive)
    (call-interactively (if (use-rtags t) 'rtags-imenu 'imenu)))

  (with-eval-after-load 'helm-gtags
    (define-key helm-gtags-mode-map (kbd "M-.") (function tags-find-symbol-at-point))
    (define-key helm-gtags-mode-map (kbd "M-,") (function tags-find-references-at-point))
    (define-key helm-gtags-mode-map (kbd "C-c g f") (function tags-find-file))
    (define-key helm-gtags-mode-map (kbd "C-c g s") (function tags-find-symbol))
    (define-key helm-gtags-mode-map (kbd "C-c g r") (function tags-find-references))
    (define-key helm-gtags-mode-map (kbd "C-c g v") (function rtags-find-virtuals-at-point))
    (define-key helm-gtags-mode-map (kbd "C-c C-j") (function tags-imenu))))

(provide 'init-rtags)
