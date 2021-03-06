(my:install-package-if-needed 'company)

(eval-after-load 'company
  '(eval-after-load 'init-irony
     '(progn
        ;; (setq company-backends (remove 'company-clang company-backends))
        ;; (add-to-list 'company-backends
        ;;              '(company-irony :with company-yasnippet company-keywords))
        (setq company-transformers '(company-sort-by-backend-importance))
        ;; (optional) adds CC special commands to `company-begin-commands' in order to
        ;; trigger completion at interesting places, such as after scope operator
        ;;     std::|
        (my:install-package-if-needed 'company-irony)
        (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))))

(with-eval-after-load 'company
  (setq company-backends (delete 'company-clang company-backends))
  (my:install-package-if-needed 'company-c-headers)
  (add-to-list 'company-backends 'company-c-headers))

(setq company-global-modes
      '(c-mode c++-mode emacs-lisp-mode makefile-mode makefile-gmake-mode))
(add-hook 'after-init-hook 'global-company-mode)

(my:use-package-ensure company-quickhelp
                       (company-quickhelp-mode 1))
(eval-after-load 'company
  '(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))

(provide 'init-company)
