;(add-to-list 'load-path (concat cedet-root-path "contrib"))

;; select which submodes we want to activate
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)

(setq semanticdb-default-system-save-directory (concat user-emacs-directory "semanticdb-system"))

;; Activate semantic
(semantic-mode 1)

;; load ia
(require 'semantic/ia)

;; customisation of modes
(defun alexott/cedet-hook ()
  ;(local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
  ;;
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)

  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  )
(add-hook 'c-mode-common-hook 'alexott/cedet-hook)
(add-hook 'lisp-mode-hook 'alexott/cedet-hook)
(add-hook 'scheme-mode-hook 'alexott/cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'alexott/cedet-hook)
(add-hook 'erlang-mode-hook 'alexott/cedet-hook)

(defun alexott/c-mode-cedet-hook ()
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-xt" 'eassist-switch-h-cpp)
  (local-set-key "\C-ce" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref)
  )
(add-hook 'c-mode-common-hook 'alexott/c-mode-cedet-hook)

(semanticdb-enable-gnu-global-databases 'c-mode t)
(semanticdb-enable-gnu-global-databases 'c++-mode t)

(when (cedet-ectag-version-check t)
  (semantic-load-enable-primary-ectags-support))

;; SRecode
;(global-srecode-minor-mode 1)

;; EDE
(global-ede-mode 1)
(ede-enable-generic-projects)

;; Setup GCC....
(require 'semantic/bovine/gcc)

;; Setup JAVA....
(require 'cedet-java)

;; Setup eassist
;(require 'eassist)

;; Init Contrib
(load-file (concat cedet-root-path (convert-standard-filename "contrib/cedet-contrib-load.el")))

(require 'lk-file-search)

(ede-cpp-root-project "samba1-7133_145"
                      :file "~/.syren/1/rel_7133_145/GNUmakefile"
                      :include-path nil
                      ;; :locate-fcn (lambda (name dir)
                      ;;               (message "NAME='%s' DIR='%s'" name dir)
                      ;;               (shell-command-to-string (concat "find " dir " -name " name))) 
                      )

;; assist eassist
(add-to-list 'eassist-header-switches '("cc" "h" "hpp" "hxx"))
(add-to-list 'eassist-header-switches '("hxx" "cc" "cxx"))
(add-to-list 'eassist-header-switches '("h" "cpp" "cc" "c" "cxx"))
(add-to-list 'eassist-header-switches '("cxx" "h" "hxx"))

(provide 'init-cedet-config)
