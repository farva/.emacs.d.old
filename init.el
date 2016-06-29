(setq candidate-custom-file (concat user-emacs-directory (convert-standard-filename "custom/basic-custom.el")))
(when (file-readable-p candidate-custom-file)
  (setq custom-file candidate-custom-file)
  (load custom-file))

(setq magit-last-seen-setup-instructions "1.4.0")

;; IMPORTANT: you must place this *before* any CEDET component (including
;; EIEIO) gets activated by another package (Gnus, auth-source, ...).
(setq cedet-root-path (concat user-emacs-directory (convert-standard-filename "repos/cedet/")))
(unless (featurep 'cedet-devel-load)
  (let ((cedet-load-file (concat cedet-root-path "cedet-devel-load.el")))
    (when (file-exists-p cedet-load-file)
      (load-file cedet-load-file))))

;; add subdirectories to load-path
(mapc
 (lambda (dir-tree) "add the directory tree to load-path"
   (let ((default-directory
           (concat user-emacs-directory
                   (convert-standard-filename (concat dir-tree "/")))))
     (normal-top-level-add-to-load-path '("."))
     (normal-top-level-add-subdirs-to-load-path)))
 '("elisp"))

(require 'cl)

;; make sure we're not running csh or some other shit...
(if (eq system-type 'gnu/linux)
    (setq shell-file-name "/bin/bash"))

;; hack for tramp to work properly
;; (require 'tramp)
;; (add-to-list 'tramp-remote-process-environment "HGPLAIN=1")

(let ((remote-shell-dir (concat user-emacs-directory (convert-standard-filename "repos/remote-shell"))))
  (when (file-exists-p remote-shell-dir)
    (add-to-list 'load-path remote-shell-dir)
    (require 'remote-shell)))

(add-to-list 'load-path (concat user-emacs-directory (convert-standard-filename "repos/markdown-mode")))
;(require 'markdown-mode)

;; scrolling
;; (global-set-key (kbd "ESC <up>") 'scroll-down-line)
;; (global-set-key (kbd "ESC <down>") 'scroll-up-line)

;; find-file-at-point
(global-set-key (kbd "C-c M-f") 'find-file-at-point)

;; compilation
(global-set-key (kbd "C-c C") 'compile)

;; (autoload 'markdown-mode "markdown-mode"
;;   "Major mode for editing Markdown files" t)
;; (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; indentation
(setq-default c-basic-offset 2)
(setq-default indent-tabs-mode nil)

;; Emacs' temporary files
;; (require 'init-temp-files)

;; WAF supprt
(setq auto-mode-alist (cons '("wscript" . python-mode) auto-mode-alist))

;; pending delete mode
(pending-delete-mode t)

;; MELPA
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
  (package-initialize))

(defvar my:package-contents-refreshed nil)
(defun my:install-package-if-needed (package)
  (when (>= emacs-major-version 24)
    (unless (package-installed-p package)
      (unless my:package-contents-refreshed
        (package-refresh-contents)
        (setq my:package-contents-refreshed t))
      (package-install package))))

(async-bytecomp-package-mode 1)

(require 'init-use-package)

;; phi-rectangle tweaks
;; (eval-after-load "phi-rectangle"
;;   '(progn
;;      (define-key phi-rectangle-mode-map (kbd "C-<return>") nil)
;;      (define-key phi-rectangle-mode-map (kbd "C-c C-SPC") 'phi-rectangle-set-mark-command)))

;; (phi-rectangle-mode)

;; JIRA REST
;; (add-to-list 'load-path (concat user-emacs-directory (convert-standard-filename "repos/jira-rest")))
;; (require 'jira-rest)

;; ORG JIRA
;; (require 'init-org-jira)

;; auto-complete
;; (require 'init-auto-complete)

;; yasnippets
(require 'init-yasnippet)

;; irony
;;(require 'init-irony)

;; ycmd
;; (require 'init-ycmd)

;; company
(require 'init-company)

;(require 'init-cedet-config)
;(require 'init-ecb)

;; EShell tweaks
;; (require 'eshell)
;; (require 'em-smart)
;; (setq eshell-where-to-jump 'begin)
;; (setq eshell-review-quick-commands nil)
;; (setq eshell-smart-space-goes-to-end t)

;; Load gtags.el
;; (require 'init-gtags)

;; Load rtags.el
;;(require 'init-rtags)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

;; site specific loads
(setq site-specific-load-file (concat user-emacs-directory (convert-standard-filename "site-lisp/init.el")))
(when (file-exists-p site-specific-load-file)
  ;; add subdirectories to load-path
  (mapc
   (lambda (dir-tree) "add the directory tree to load-path"
     (let ((default-directory
             (concat user-emacs-directory
                     (convert-standard-filename "site-lisp/")
                     (convert-standard-filename (concat dir-tree "/")))))
       (when (file-exists-p default-directory)
         (normal-top-level-add-to-load-path '("."))
         (normal-top-level-add-subdirs-to-load-path))))
   '("elisp"))

  (load-file site-specific-load-file))

;; transpose frame
(require 'init-transpose-frame)

;; whitespace
(require 'init-whitespace)

;; which function mode
(require 'init-which-func)

;; load theme
(require 'init-theme)

;; load mark fixes
;; (http://www.masteringemacs.org/articles/2010/12/22/fixing-mark-commands-transient-mark-mode/)
(require 'init-mark-fixes)

;; load projectile
(require 'init-projectile)

;; load auto-yasnippet
(require 'init-auto-yasnippet)

;; cua
;; (require 'init-cua)

;; SLIME
(require 'init-slime)

;; Eshell
(require 'eshell-fixes)

;; smartparens
(require 'init-smartparens)

;; cider
(require 'init-cider)

;; ggtags
;; (require 'init-ggtags)

;; eclim
(require 'init-eclim)

;; grep-a-lot
(use-package grep-a-lot
  :ensure t)
(grep-a-lot-setup-keys)

;; helm general
(require 'init-helm)

;; helm-gtags
(require 'init-helm-gtags)

;; helm-swoop
(require 'init-helm-swoop)

;; elpy
(require 'init-elpy)

;; virtualenvwrapper
(require 'init-virtualenvwrapper)

;; org
(require 'init-org)

;; multi-term
(require 'init-multi-term)

;; extra packages
(require 'init-extra-packages)

;; expand region
(require 'init-expand-region)
