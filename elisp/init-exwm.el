(require 'exwm)
(require 'exwm-config)

;; Emacs server is not required to run EXWM but it has some interesting uses
;; (see next section)
(server-start)

(advice-add 'exwm-config-ido :override #'ignore)
(exwm-config-default)

(require 'exwm-systemtray)
(exwm-systemtray-enable)

;; Time in modeline (since I can't find something for systray)
(setq display-time-day-and-date nil)
(display-time-mode 1)

(defun my:exwm-run-eshell-command (command)
  (interactive (list (read-eshell-command "(Eshell)$ ")))
  (start-process-eshell-command nil command))

(provide 'init-exwm)
