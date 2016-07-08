(my:use-package-ensure elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))


(provide 'init-elisp-slime-nav)
