;; (set-language-environment "utf-8")

;; (add-to-list 'load-path "~/src/slime/")  ;or wherever you put it

;; ;;; Note that if you save a heap image, the character
;; ;;; encoding specified on the command line will be preserved,
;; ;;; and you won't have to specify the -K utf-8 any more.
;; (setq inferior-lisp-program "/usr/local/bin/ccl64 -K utf-8")

;; (require 'slime)
;; (setq slime-net-coding-system 'utf-8-unix)
;; (slime-setup '(slime-fancy))

(eval-after-load 'slime
  '(setq inferior-lisp-program (expand-file-name "~/ccl/lx86cl64"))
  '(slime-setup '(slime-fancy slime-company)))

(provide 'init-slime)
