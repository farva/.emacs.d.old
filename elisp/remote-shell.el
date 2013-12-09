(require 'my-common)

(defun remote-shell (machine user protocol &optional port)
  "Opens a shell to remote machine using TRAMP"
  (interactive "sMachine: \nsUser: \nsProtocol: \nsPort: ")
  (with-temp-buffer
    (cd (concat "/" protocol ":" user "@" machine
		(if (and port (string-natural-int-p port))
		    (concat "#" port)
		  "")
		":~/"))
    (shell (concat "*" machine "*"))))

(provide 'remote-shell)
