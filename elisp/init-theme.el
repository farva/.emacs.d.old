(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (load-theme 'hc-zenburn t))))
  (load-theme 'hc-zenburn t))

(provide 'init-theme)
