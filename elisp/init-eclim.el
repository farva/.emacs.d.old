(my:install-package-if-needed 'emacs-eclim)

(require 'eclim)
(global-eclim-mode)

;; control the eclim daemon from emacs
(require 'eclimd)

;; company mode
;; (eval-after-load 'company
;;   '(progn
;;      (require 'company-emacs-eclim)
;;      (company-emacs-eclim-setup)
;;      ;; (global-company-mode t)
;;      ))

;; use Xvfb
;; (defun start-eclimd-ad (start-eclimd-func &rest r)
;;   (call-process "Xvfb" nil 0 nil ":1 -screen 0 1024x768x24")
;;   (let ((display-orig (getenv "DISPLAY")))
;;     (setenv "DISPLAY" ":1")
;;     (apply start-eclimd-func r)
;;     (setenv "DISPLAY" display-orig)))
;; (advice-add 'start-eclimd :around #'start-eclimd-ad)

(defun eclim/c-call-hierarchy (project file offset length encoding)
  (eclim--call-process "c_callhierarchy"
                       "-p" project
                       "-f" file
                       "-o" (number-to-string offset)
                       "-l" (number-to-string length)
                       "-e" encoding))

(defun eclim--c-identifier-at-point (&optional full position)
  "Currently just calls the `java' version"
  (eclim--java-identifier-at-point full position))

(defun eclim-c-call-hierarchy (project file encoding)
  (interactive (list (eclim-project-name)
                     (eclim--project-current-file)
                     (eclim--current-encoding)))
  ;; (interactive (interactive-form 'eclim-java-call-hierarchy))
  (cl-letf (((symbol-function 'eclim/java-call-hierarchy)
             #'eclim/c-call-hierarchy))
    (eclim-java-call-hierarchy project file encoding)))

(provide 'init-eclim)
