(add-to-list 'load-path (concat user-emacs-directory (convert-standard-filename "repos/org-jira")))

(setq jiralib-url "https://jira.xiv.ibm.com")

(require 'org-jira)
