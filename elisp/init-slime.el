;; (set-language-environment "utf-8")

;; (add-to-list 'load-path "~/src/slime/")  ;or wherever you put it

;; ;;; Note that if you save a heap image, the character
;; ;;; encoding specified on the command line will be preserved,
;; ;;; and you won't have to specify the -K utf-8 any more.
;; (setq inferior-lisp-program "/usr/local/bin/ccl64 -K utf-8")

;; (require 'slime)
;; (setq slime-net-coding-system 'utf-8-unix)
;; (slime-setup '(slime-fancy))

(my:install-package-if-needed 'slime)
(my:install-package-if-needed 'slime-company)

(eval-after-load 'slime
  '(let ((inferior-lisp-prog-path (expand-file-name "~/ccl/lx86cl64")))
     (when (file-exists-p inferior-lisp-prog-path)
       (setq inferior-lisp-program inferior-lisp-prog-path)
       (slime-setup '(slime-fancy slime-company)))))

(provide 'init-slime)
