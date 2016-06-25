(my:use-package-ensure multi-term)

(add-to-list 'term-bind-key-alist
             '("C-c C-j" . term-line-mode))

(provide 'init-multi-term)
