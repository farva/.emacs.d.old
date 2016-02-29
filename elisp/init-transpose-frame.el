(use-package transpose-frame
  :ensure t)

(let ((prefix "\C-ct"))
  (global-set-key (concat prefix "t") 'transpose-frame)
  (global-set-key (concat prefix "v") 'flip-frame)
  (global-set-key (concat prefix "h") 'flop-frame)
  (global-set-key (concat prefix ">") 'rotate-frame-clockwise)
  (global-set-key (concat prefix "<") 'rotate-frame-anticlockwise))

(provide 'init-transpose-frame)
