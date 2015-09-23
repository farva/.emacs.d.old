;; (defun eshell/sudo (&rest args)
;;   (message "eshell/sudo is in the house with %S" args)
;;   (throw 'eshell-replace-command
;;           (concat
;;            (eshell-quote-argument "sudo ")
;;            (eshell-parse-command
;;             (car args)
;;             (cdr args)))))

;; (defun eshell/sudo (&rest args)
;;   (eshell-named-command "*sudo" args))

;; (eval-after-load 'eshell
;;   '(add-to-list 'eshell-visual-commands "alsamixer"))

(require 'em-tramp)

(setq password-cache t) ; enable password caching
(setq password-cache-expiry 3600) ; for one hour (time in secs)

(provide 'eshell-fixes)
