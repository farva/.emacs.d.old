;; -*- lexical-binding: t; -*-

(require 'my-common)

(defun remote-shell (machine user shell protocol &optional port)
  "Opens a shell to remote machine using TRAMP"
  (interactive "sMachine: \nsUser: \nsShell: \nsProtocol: \nsPort: ")
  (with-temp-buffer
    (cd (concat "/" protocol ":" user "@" machine
		(if (and port (string-natural-int-p port))
		    (concat "#" port)
		  "")
		":~/"))
    (let ((explicit-shell-file-name (concat "/bin/" shell)))
      (shell (concat "*" machine "*")))))

(defvar remote-shell-edit-tmp-file-dmz-string "-<0=0>-"
  "Prefix and suffix added to the temporary file needed by remote EDITOR.
This will act as a DMZ to distinguish it from other text in the buffer.")

; TODO: look at comint-preoutput-filter-functions. maybe can be done without user intervention.'
(defun attach-current-remote-editing ()
  "Attach to a remote editing session of the current buffer."
  (interactive)
  (let* ((tmp-file-local-path
	  (save-excursion
	    (goto-char (point-max))
	    (re-search-backward (concat remote-shell-edit-tmp-file-dmz-string "\\(.+\\)" remote-shell-edit-tmp-file-dmz-string))
	    (match-string 1)))
	 (base-path (if (char-equal ?/ (string-to-char tmp-file-local-path))
			(progn
			  (string-match "\\(^/[^/]+\\)/" default-directory)
			  (match-string-no-properties 1 default-directory))
		      default-directory))
	 (tmp-file-tramp-path (concat base-path tmp-file-local-path)))
    (start-process "remote-edit" nil "emacsclient" tmp-file-tramp-path)
    ;(comint-send-input)
    (message "Editing '%s'." tmp-file-tramp-path)))

(provide 'remote-shell)
