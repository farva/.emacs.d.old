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
        (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
        (add-to-list 'company-backends 'company-c-headers))))

(setq company-global-modes
      '(c-mode c++-mode emacs-lisp-mode makefile-mode makefile-gmake-mode))
(add-hook 'after-init-hook 'global-company-mode)

(provide 'init-company)
