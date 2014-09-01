(setq rtags-src-dir (concat user-emacs-directory (convert-standard-filename "repos/rtags/src/")))

(load (concat rtags-src-dir "rtags"))

(defun use-rtags (&optional useFileManager)
  (and (rtags-executable-find "rc")
       (cond ((not (gtags-get-rootpath)) t)
             ((and (not (eq major-mode 'c++-mode))
                   (not (eq major-mode 'c-mode))) (rtags-has-filemanager))
             (useFileManager (rtags-has-filemanager))
             (t (rtags-is-indexed)))))

(defun tags-find-symbol-at-point (&optional prefix)
  (interactive "P")
  (if (or prefix
          (and (not (rtags-find-symbol-at-point))
               rtags-last-request-not-indexed))
      (let (current-prefix-arg)
        (call-interactively 'ggtags-find-tag-dwim))))
(defun tags-find-references-at-point (&optional prefix)
  (interactive "P")
  (if (or prefix
          (and (not (rtags-find-references-at-point))
               rtags-last-request-not-indexed))
      (let (current-prefix-arg)
        (call-interactively 'ggtags-find-reference))))
(defun tags-find-symbol (&optional prefix)
  (interactive "P")
  (if (or prefix (not (use-rtags)))
      (let ((current-prefix-arg t))
        (call-interactively 'ggtags-find-definition))
    (call-interactively 'rtags-find-symbol)))
(defun tags-find-references (&optional prefix)
  (interactive "P")
  (if (or prefix (not (use-rtags)))
      (let ((current-prefix-arg t))
        (call-interactively 'ggtags-find-reference))
    (call-interactively 'rtags-find-references)))
(defun tags-find-file (&optional prefix)
  (interactive "P")
  (call-interactively (if (or prefix (not (use-rtags t)))
                          'ggtags-find-file
                        'rtags-find-file)))
(defun tags-imenu ()
  (interactive)
  (call-interactively (if (use-rtags t) 'rtags-imenu 'idomenu)))

(defadvice ring-insert (before push-location-to-rtags-stack (ring item))
  "Push location, if relevant, to RTags stack using `rtags-location-stack-push'.
\"Relevant location\" is based on:
1. `find-tag-marker-ring' exists (ggtags started).
2. `ring' is in fact `find-tag-marker-ring'.
3. `item' is a marker."
  (when (and (boundp 'find-tag-marker-ring)
             (eq ring find-tag-marker-ring)
             (markerp item))
    (with-current-buffer (marker-buffer item)
      (save-excursion
        (goto-char (marker-position item))
        (rtags-location-stack-push)))))
(ad-activate 'ring-insert)

(let ((prefix "\C-cr"))
  (define-key c-mode-base-map (kbd "M-.") (function tags-find-symbol-at-point))
  (define-key c-mode-base-map (kbd "M-,") (function tags-find-references-at-point))
  (define-key c-mode-base-map (concat prefix "f") (function tags-find-file))
  (define-key c-mode-base-map (concat prefix ".") (function tags-find-symbol))
  (define-key c-mode-base-map (concat prefix ",") (function tags-find-references))
  (define-key c-mode-base-map (concat prefix "v") (function rtags-find-virtuals-at-point))
  (define-key c-mode-base-map (kbd "M-i") (function tags-imenu))
  (define-key c-mode-base-map (kbd "C-c <") (function rtags-location-stack-back))
  (define-key c-mode-base-map (kbd "C-c >") (function rtags-location-stack-forward))
  (define-key c-mode-base-map (kbd "M-n") (function rtags-next-match))
  (define-key c-mode-base-map (kbd "M-p") (function rtags-previous-match)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

(eval-after-load "ggtags"
  '(progn
     (define-key ggtags-mode-map (kbd "C-M-.") nil)
     (define-key ggtags-mode-map (kbd "C-x M-.") 'ggtags-find-tag-regexp)
     (define-key ggtags-mode-map (kbd "M-.") (function tags-find-symbol-at-point))
     (define-key ggtags-mode-map (kbd "M-,") (function tags-find-references-at-point))))

(provide 'init-rtags)
