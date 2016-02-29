(my:install-package-if-needed 'auto-yasnippet)

(let ((prefix "\C-ca"))
  (global-set-key (concat prefix "c") 'aya-create)
  (global-set-key (concat prefix "e") 'aya-expand))

(provide 'init-auto-yasnippet)
