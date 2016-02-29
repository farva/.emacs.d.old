;;(setq x-select-enable-clipboard t)

;; Callback for when user cuts
(defun xsel-cut-function (text &optional push)
  ;; Insert text to temp-buffer, and "send" content to xsel stdin
  (with-temp-buffer
    (insert text)
    ;; I prefer using the "clipboard" selection (the one the
    ;; typically is used by c-c/c-v) before the primary selection
    ;; (that uses mouse-select/middle-button-click)
    (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
;; Call back for when user pastes
(defun xsel-paste-function()
  ;; Find out what is current selection by xsel. If it is different
  ;; from the top of the kill-ring (car kill-ring), then return
  ;; it. Else, nil is returned, so whatever is in the top of the
  ;; kill-ring will be used.
  (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
    (unless (string= (car kill-ring) xsel-output)
      xsel-output )))

(defun yank-to-x-clipboard ()
  (interactive)
  (if (region-active-p)
        (progn
          (call-process-region (region-beginning) (region-end) "xsel" nil 0 nil "--clipboard" "--input")
          ;; (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
          (message "Yanked region to clipboard!")
          (deactivate-mark))
    (message "No region active; can't yank to clipboard!")))

;; If emacs is run in a terminal, the clipboard- functions have no
;; effect. Instead, we use of xsel, see
;; http://www.vergenet.net/~conrad/software/xsel/ -- "a command-line
;; program for getting and setting the contents of the X selection"
(defun my-copy-with-x ()
  (interactive)
  (unless window-system
    (when (getenv "DISPLAY")    
      ;; Attach callbacks to hooks
      (let ((interprogram-cut-function 'xsel-cut-function))
        ;; (setq interprogram-paste-function 'xsel-paste-function)
        (kill-ring-save (region-beginning) (region-end))
        (message "Yanked region to clipboard!")
        ;; Idea from
        ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
        ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
        ))))

(global-set-key "\C-c\C-w" 'yank-to-x-clipboard)
(global-set-key "\C-c\M-w" 'my-copy-with-x)
(global-set-key "\C-c\C-y" 'clipboard-yank)
