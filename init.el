(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-auto-revert-mode t)
 '(gnutls-min-prime-bits 256)
 '(jabber-account-list (quote (("orenbaracha@gmail.com/emacs" (:network-server . "talk.google.com") (:port . 443) (:connection-type . ssl))))))
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

(require 'cl)

(add-to-list 'load-path (concat user-emacs-directory (convert-standard-filename "repos/remote-shell")))
(require 'remote-shell)

(add-to-list 'load-path (concat user-emacs-directory (convert-standard-filename "repos/markdown-mode")))
;(require 'markdown-mode)

;; scrolling
(global-set-key (kbd "ESC <up>") 'scroll-down-line)
(global-set-key (kbd "ESC <down>") 'scroll-up-line)

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
    '(auto-complete auto-complete-c-headers ggtags jabber phi-rectangle yasnippet)
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
(eval-after-load "phi-rectangle"
  '(progn
     (define-key phi-rectangle-mode-map (kbd "C-<return>") nil)
     (define-key phi-rectangle-mode-map (kbd "C-c C-SPC") 'phi-rectangle-set-mark-command)))

(phi-rectangle-mode)

;; JIRA REST
(add-to-list 'load-path (concat user-emacs-directory (convert-standard-filename "repos/jira-rest")))
(require 'jira-rest)

;; ORG JIRA
(require 'init-org-jira)

;; ggtags tweaks
(eval-after-load "ggtags"
  '(progn
     (define-key ggtags-mode-map (kbd "C-M-.") nil)
     (define-key ggtags-mode-map (kbd "C-x M-.") 'ggtags-find-tag-regexp)))

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
