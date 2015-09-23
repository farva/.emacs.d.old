(eval-after-load 'company
  '(eval-after-load 'cider
     '(progn
        (add-hook 'cider-repl-mode-hook #'company-mode)
        (add-hook 'cider-mode-hook #'company-mode))))

(add-hook 'cider-mode-hook #'eldoc-mode)

;; Suppress auto-enabling of cider-mode in clojure-mode buffers, when starting CIDER
(setq cider-auto-mode nil)

;; syntax highlight everything, not just from `clojure.core'
(setq cider-font-lock-dynamically '(macro core function var))

(add-hook 'cider-repl-mode-hook #'subword-mode)

(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

(add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)

(require 'cider-eval-sexp-fu)

(provide 'init-cider)
