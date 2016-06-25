;; Show the current function name in the header line
(which-function-mode)

(defun which-func-set-header-format ()
  (when (listp header-line-format)
    (add-to-list 'header-line-format
                 '("" which-func-format " "))))
(add-hook 'prog-mode-hook #'which-func-set-header-format)
(add-hook 'diff-mode-hook #'which-func-set-header-format)

(setq mode-line-misc-info
      ;; We remove Which Function Mode from the mode line,
      ;; because it's mostly invisible here anyway.
      (assq-delete-all 'which-func-mode mode-line-misc-info))

(provide 'init-which-func)
