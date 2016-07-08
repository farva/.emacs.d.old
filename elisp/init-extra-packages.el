(my:use-package-ensure xquery-tool)
(my:use-package-ensure helm-mt)
(my:use-package-ensure helm-ag)
(my:use-package-ensure auto-yasnippet)
(my:use-package-ensure anaphora)
;; (my:use-package-ensure region-bindings-mode
;;                        (region-bindings-mode-enable))
(my:use-package-ensure grep-a-lot
                       (grep-a-lot-setup-keys))
(my:use-package-ensure pdf-tools
                       (pdf-tools-install))

(provide 'init-extra-packages)
