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

(provide 'init-auto-complete)
