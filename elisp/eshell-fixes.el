;; -*- lexical-binding: t -*-

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

(defun my-eshell-kill (&rest args)
  "Kill processes.
Usage: kill [-<signal>] <pid>|<process> ...
Accepts PIDs and process objects."
  ;; If the first argument starts with a dash, treat it as the signal
  ;; specifier.
  (let ((signum 'SIGINT))
    (let ((arg (pp-to-string (car args)))
          (case-fold-search nil))
      (when (stringp arg)
        (cond
         ((string-match "\\`-[[:digit:]]+\\'" arg)
          (setq signum (abs (string-to-number arg))))
         ((string-match "\\`-\\([[:upper:]]+\\|[[:lower:]]+\\)\\'" arg)
          (setq signum (abs (string-to-number arg)))))
        (setq args (cdr args))))
    (while args
      (let ((arg (if (eshell-processp (car args))
                     (process-id (car args))
                   (car args))))
        (when arg
          (cond
           ((null arg)
            (error "kill: null pid.  Process may actually be a network connection."))
           ((not (numberp arg))
            (error "kill: invalid argument type: %s" (type-of arg)))
           ((and (numberp arg)
                 (<= arg 0))
            (error "kill: bad pid: %d" arg))
           (t
            (signal-process arg signum)))))
      (setq args (cdr args))))
  nil)
(advice-add 'eshell/kill :override #'my-eshell-kill)

(require 'em-tramp)

(setq password-cache t) ; enable password caching
(setq password-cache-expiry 3600) ; for one hour (time in secs)

(add-hook 'eshell-mode-hook
          (lambda ()
            (add-to-list 'eshell-visual-subcommands '("systemctl" "status"))))

(defun read-eshell-command (prompt &optional initial-contents &rest args)
  "Read an Eshell command from minibuffer."
  (let ((eshell-non-interactive-p t)
        command)
    (minibuffer-with-setup-hook #'(lambda ()
                                    (eshell-mode)
                                    (eshell-return-exits-minibuffer))
      (setq command
            (apply 'read-from-minibuffer
                   prompt nil nil nil nil
                   initial-contents args))
      (when (eshell-using-module 'eshell-hist)
        (eshell-add-input-to-history command))
      command)))

(defun start-process-eshell-command (buffer command)
  "Start a program in an Eshell subprocess."
  (async-start
   (lambda ()
     (eshell-command command))
   (if buffer
       (lambda (result)
         (with-current-buffer buffer
           (save-excursion
             (insert result))))
     'ignore)))

(provide 'eshell-fixes)
