(setq custom-file (concat user-emacs-directory (convert-standard-filename "custom/basic-custom.el")))
(load custom-file)

;; IMPORTANT: you must place this *before* any CEDET component (including
;; EIEIO) gets activated by another package (Gnus, auth-source, ...).
(setq cedet-root-path (concat user-emacs-directory (convert-standard-filename "repos/cedet/")))
(unless (featurep 'cedet-devel-load)
  (load-file (concat cedet-root-path "cedet-devel-load.el")))

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

(add-to-list 'load-path (concat user-emacs-directory (convert-standard-filename "repos/remote-shell")))
(require 'remote-shell)

(add-to-list 'load-path (concat user-emacs-directory (convert-standard-filename "repos/markdown-mode")))
;(require 'markdown-mode)

;; scrolling
(global-set-key (kbd "ESC <up>") 'scroll-down-line)
(global-set-key (kbd "ESC <down>") 'scroll-up-line)

;; find-file-at-point
(global-set-key (kbd "C-c M-f") 'find-file-at-point)

;; compilation
(global-set-key (kbd "C-c C") 'compile)

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

  (package-initialize)

  (defvar prelude-packages
    '(auto-complete auto-complete-c-headers company-irony ggtags irony jabber magit phi-rectangle yasnippet)
    "A list of packages to ensure are installed at launch.")

  (defun prelude-packages-installed-p ()
    "Check if all packages in `prelude-packages' are installed."
    (every #'package-installed-p prelude-packages))

  (defun prelude-require-package (package)
    "Install PACKAGE unless already installed."
    (unless (memq package prelude-packages)
      (add-to-list 'prelude-packages package))
    (unless (package-installed-p package)
      (package-install package)))

  (defun prelude-require-packages (packages)
    "Ensure PACKAGES are installed.
Missing packages are installed automatically."
    (mapc #'prelude-require-package packages))

  (define-obsolete-function-alias 'prelude-ensure-module-deps 'prelude-require-packages)

  (defun prelude-install-packages ()
    "Install all packages listed in `prelude-packages'."
    (unless (prelude-packages-installed-p)
      ;; check for new packages (package versions)
      (message "%s" "Emacs Prelude is now refreshing its package database...")
      (package-refresh-contents)
      (message "%s" " done.")
      ;; install the missing packages
      (prelude-require-packages prelude-packages)))

  ;; run package installation
  (prelude-install-packages)
  )

;; phi-rectangle tweaks
;; (eval-after-load "phi-rectangle"
;;   '(progn
;;      (define-key phi-rectangle-mode-map (kbd "C-<return>") nil)
;;      (define-key phi-rectangle-mode-map (kbd "C-c C-SPC") 'phi-rectangle-set-mark-command)))

;; (phi-rectangle-mode)

;; enable CUA just for what I want
(cua-selection-mode t)
;;(setq cua-enable-cua-keys nil)
;;(setq cua-highlight-region-shift-only t) ;; no transient mark mode
;;(setq cua-toggle-set-mark nil) ;; original set-mark behavior, i.e. no transient-mark-mode
;;(cua-mode)
(define-key cua-global-keymap (kbd "C-<return>") nil)
(define-key cua-global-keymap (kbd "C-c C-SPC") 'cua-set-rectangle-mark)

;; JIRA REST
(add-to-list 'load-path (concat user-emacs-directory (convert-standard-filename "repos/jira-rest")))
(require 'jira-rest)

;; ORG JIRA
(require 'init-org-jira)

;; auto-complete
(require 'auto-complete)
; default config
(require 'auto-complete-config)
(ac-config-default)
; let's define a function which initializes auto-complete-c-headers and gets called for c/c++ hooks
(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  ; if the needs arise, do: `gcc -xc++ -E -v -', and then add like this:
  ;(add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-redhat-linux/4.1.2/../../../../include/c++/4.1.2")
  )
; now let's call this function from c/c++ hooks
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

;; yasnippets
(require 'yasnippet)
(yas-global-mode 1)

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

;; add to auto-complete
(require 'ac-company)
(ac-company-define-source ac-source-company-irony company-irony)

(add-to-list 'load-path (concat user-emacs-directory (convert-standard-filename "repos/ac-irony")))
(require 'ac-irony)
(defun my-ac-irony-hook ()
  ;; be cautious, if yas is not enabled before (auto-complete-mode 1), overlays
  ;; *may* persist after an expansion.
  ;(yas-minor-mode 1)
  ;(auto-complete-mode 1)

  (add-to-list 'ac-sources 'ac-source-irony)
  (define-key irony-mode-map (kbd "M-RET") 'ac-complete-irony-async))
(add-hook 'irony-mode-hook 'my-ac-irony-hook)

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
(require 'init-rtags)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

;; site specific loads
(setq site-specific-load-file (concat user-emacs-directory (convert-standard-filename "site-lisp/init.el")))
(when (file-exists-p site-specific-load-file)
  (load-file site-specific-load-file))

;; transpose frame
(require 'init-transpose-frame)

;; whitespace
(require 'init-whitespace)

;; projectile-mode
;; (projectile-global-mode)
