(defun string-natural-int-p (string)
  (if (string-match "\\`[0-9]+\\'" string)
      t
    nil))

(provide 'my-common)
