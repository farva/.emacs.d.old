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

;; add subdirectories to load-path
(mapc
 (lambda (dir-tree) "add the directory tree to load-path"
   (let ((default-directory
           (concat user-emacs-directory
                   (convert-standard-filename (concat dir-tree "/")))))
     (normal-top-level-add-to-load-path '("."))
     (normal-top-level-add-subdirs-to-load-path)))
 '("elisp"))

(add-to-list 'load-path (concat user-emacs-directory (convert-standard-filename "repos/remote-shell")))
(require 'remote-shell)

(add-to-list 'load-path (concat user-emacs-directory (convert-standard-filename "repos/markdown-mode")))
;(require 'markdown-mode)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; indentation
(setq-default c-basic-offset 2)
(setq-default indent-tabs-mode nil)

;; WAF supprt
(setq auto-mode-alist (cons '("wscript" . python-mode) auto-mode-alist))

;; MELPA
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

  (defvar prelude-packages
    '(ggtags phi-rectangle)
    "A list of packages to ensure are installed at launch.")

  (defun prelude-packages-installed-p ()
    (loop for p in prelude-packages
          when (not (package-installed-p p)) do (return nil)
          finally (return t)))

  (unless (prelude-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs Prelude is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (dolist (p prelude-packages)
      (when (not (package-installed-p p))
        (package-install p))))

  (package-initialize)
  )

;; phi-rectangle tweaks
(eval-after-load "phi-rectangle"
  '(progn
     (define-key phi-rectangle-mode-map (kbd "C-<return>") nil)
     (define-key phi-rectangle-mode-map (kbd "C-c C-SPC") 'phi-rectangle-set-mark-command)))

(phi-rectangle-mode)

;; JIRA REST
(add-to-list 'load-path (concat user-emacs-directory (convert-standard-filename "repos/jira-rest")))
(require 'jira-rest)
