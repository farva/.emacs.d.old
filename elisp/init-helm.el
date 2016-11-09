(my:install-package-if-needed 'helm)
(my:install-package-if-needed 'helm-flx)
(my:install-package-if-needed 'helm-fuzzier)
(my:install-package-if-needed 'helm-descbinds)

(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

;; helm-M-x
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t)

;; kill ring
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; helm mini
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

;; helm find-file
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; helm locate
;; (setq helm-locate-fuzzy-match t)

;; helm occur
(global-set-key (kbd "C-c h o") 'helm-occur)

;; helm mark ring
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

;; helm eval-expression
(global-set-key (kbd "C-c h M-:") 'helm-eval-expression-with-eldoc)

(helm-mode 1)
(helm-flx-mode +1)
(require 'helm-fuzzier)
(helm-fuzzier-mode 1)
(helm-autoresize-mode t)

;; Eshell history
(require 'helm-eshell)
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))

;; shell history
(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)

;; minibuffer history
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

;; descbinds
(require 'helm-descbinds)
(helm-descbinds-mode)

;; my way of opening helm window
(defvar before-helm-display-buffer-alist nil)
;;
(defun my:restore-display-buffer-alist ()
  "restore `display-buffer-alist' from `before-helm-display-buffer-alist'"
  (setq display-buffer-alist before-helm-display-buffer-alist))
;;
(defun my:helm-restore-and-display (buffer alist)
  (my:restore-display-buffer-alist)
  (display-buffer-in-side-window buffer alist))
;;
(defun my:display-helm-at-bottom ()
  ;; backup `display-buffer-alist'
  (setq before-helm-display-buffer-alist display-buffer-alist)
  ;; add magic
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helm" (* not-newline) "*" eos)
                 (my:helm-restore-and-display)
                 (inhibit-same-window . t)
                 (window-height . 0.4))))
;;
(add-hook 'helm-after-initialize-hook 'my:display-helm-at-bottom)

;; Other way: https://www.reddit.com/r/emacs/comments/33qj0p/make_helm_window_always_at_the_bottom_using/
;;
;; disable popwin-mode in an active Helm session It should be disabled
;; otherwise it will conflict with other window opened by Helm persistent
;; action, such as *Help* window.
;; (with-eval-after-load 'init-popwin
;;   (push '("^\*helm.+\*$" :regexp t) popwin:special-display-config)
;;   (add-hook 'helm-after-initialize-hook (lambda ()
;;                                           (popwin:display-buffer helm-buffer t)
;;                                           (popwin-mode -1)))
;;   ;;  Restore popwin-mode after a Helm session finishes.
;;   (add-hook 'helm-cleanup-hook (lambda () (popwin-mode 1))))

;; fix switch-buffer-other-window with completion
(add-to-list 'helm-completing-read-handlers-alist
             '(switch-to-buffer-other-window . helm-completing-read-with-cands-in-buffer))

;; for man
(add-to-list 'helm-completing-read-handlers-alist
             '(man . helm-completing-read-with-cands-in-buffer))

;; make `company' bindings as helm's
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

;; HACK
(setq helm-follow-mode nil)

(provide 'init-helm)
