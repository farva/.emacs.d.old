(defun start-hide-ifdef ()
  (hide-ifdef-mode t)
  (hide-ifdefs))

(add-hook 'c-mode-hook 'start-hide-ifdef)
(add-hook 'c++-mode-hook 'start-hide-ifdef)

(provide 'init-hide-ifdef)
