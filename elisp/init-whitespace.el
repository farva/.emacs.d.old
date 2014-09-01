(require 'whitespace)

(setq whitespace-style
      '(face tabs trailing lines-tail space-before-tab indentation space-after-tab))
(setq whitespace-global-modes
      '(c-mode c++-mode lisp-mode emacs-lisp-mode perl-mode python-mode sh-mode java-mode makefile-mode))
(global-whitespace-mode)

(provide 'init-whitespace)
