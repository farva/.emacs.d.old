(my:install-package-if-needed 'org)
(my:install-package-if-needed 'graphviz-dot-mode)

(setq org-babel-load-languages '((dot . t) (emacs-lisp . t)))

;; fix conflicts with `yasnippt' key bindings
(defun unset-yas-bindings ()
  "disable <C-c &> prefix"
  (define-key yas-minor-mode-map
    (kbd "C-c &") nil))
(eval-after-load 'yasnippet
  '(add-hook 'org-mode-hook
             'unset-yas-bindings))

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c a") 'org-agenda))

(provide 'init-org)
