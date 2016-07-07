(my:use-package-ensure magit)

(defun helm-completing-read-with-cands-no-preselect
    (prompt collection test require-match
     init hist default inherit-input-method
     name buffer)
  "Same as `helm-completing-read-default-1' but use candidates-in-buffer."
  ;; Some commands like find-tag may use `read-file-name' from inside
  ;; the calculation of collection. in this case it clash with
  ;; candidates-in-buffer that reuse precedent data (files) which is wrong.
  ;; So (re)calculate collection outside of main helm-session.
  (let ((cands (all-completions (or init "") collection)))
    (helm-completing-read-default-1 prompt cands test 'config
                                    init hist default inherit-input-method
                                    name buffer t)))

;; magit fixes
;; (add-to-list 'helm-completing-read-handlers-alist
;;              '(magit-checkout . helm-completing-read-with-cands-no-preselect)
;;              ;; '(magit-checkout . helm-completing-read-default-1)
;;              )

(provide 'init-magit)
