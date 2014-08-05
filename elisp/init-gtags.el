(autoload 'gtags-mode "gtags" "" t)
(add-hook 'gtags-select-mode-hook
          '(lambda ()
             (setq hl-line-face 'underline)
             (hl-line-mode 1)
             ))

(gtags-mode)

(provide 'init-gtags)
