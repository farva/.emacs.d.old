(my:install-package-if-needed 'emacs-eclim)
(my:use-package-ensure origami)

(require 'eclim)
(global-eclim-mode)

;; control the eclim daemon from emacs
(require 'eclimd)

;; company mode
;; (with-eval-after-load 'company
;;   (require 'company-emacs-eclim)
;;   (company-emacs-eclim-setup)
;;   ;; (global-company-mode t)
;;   )

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

(defun eclim-c-call-hierarchy ()
  (interactive)
  (cl-letf (((symbol-function 'eclim/java-call-hierarchy)
             #'eclim/c-call-hierarchy))
    (call-interactively #'eclim-java-call-hierarchy)))

(define-key eclim-mode-map (kbd "C-c C-e c h")
  (lambda ()
    (interactive)
    (let (call-hierarchy-func)
      (cl-letf (((symbol-function 'call-hierarchy-func)
                 (if (or (eq major-mode 'c-mode)
                         (eq major-mode 'c++-mode))
                     #'eclim-c-call-hierarchy
                   #'eclim-java-call-hierarchy)))
        (call-interactively 'call-hierarchy-func)))))

(define-key origami-mode-map (kbd "<tab>") #'origami-toggle-node)
(define-key origami-mode-map (kbd "<S-tab>") #'origami-recursively-toggle-node)

(defun my:add-origami-folding (&optional ret)
  (origami-mode)
  (origami-close-all-nodes (current-buffer))
  (origami-open-node (current-buffer) (point-min))
  ret)

(advice-add 'eclim--java-insert-call-hierarchy-node
            :filter-return #'my:add-origami-folding)

(provide 'init-eclim)
