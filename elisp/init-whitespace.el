(require 'whitespace)

(setq whitespace-style
      '(face trailing lines-tail space-before-tab indentation space-after-tab))
(setq whitespace-global-modes
      '(c-mode c++-mode lisp-mode emacs-lisp-mode perl-mode python-mode sh-mode java-mode makefile-mode))
;;(global-whitespace-mode)

(defun add-ws-mode-to-buffer ()
  "Adds `whitespace-mode' to buffer after local varaibles"
  (add-hook 'hack-local-variables-hook #'whitespace-mode nil t))

(add-hook 'c-mode-common-hook #'add-ws-mode-to-buffer)
(add-hook 'lisp-mode-hook #'add-ws-mode-to-buffer)
(add-hook 'emacs-lisp-mode-hook #'add-ws-mode-to-buffer)

(provide 'init-whitespace)
