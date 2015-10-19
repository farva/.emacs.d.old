(require 'eclim)
(global-eclim-mode)

;; control the eclim daemon from emacs
(require 'eclimd)

;; company mode
(eval-after-load 'company
  '(progn
     (require 'company-emacs-eclim)
     (company-emacs-eclim-setup)
     ;; (global-company-mode t)
     ))

;; use Xvfb
(defun start-eclimd-ad (start-eclimd-func &rest r)
  (call-process "Xvfb" nil 0 nil ":1 -screen 0 1024x768x24")
  (let ((display-orig (getenv "DISPLAY")))
    (setenv "DISPLAY" ":1")
    (apply start-eclimd-func r)
    (setenv "DISPLAY" display-orig)))
;; (advice-add 'start-eclimd :around #'start-eclimd-ad)

(provide 'init-eclim)
