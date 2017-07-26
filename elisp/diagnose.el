
;;; diagnose.el --- Core customization routines for dlisp

;; Copyright (C) 2006-2016 Davin Pearson

;; Author/Maintainer: Davin Max Pearson <http://davin.50webs.com>
;; Keywords: Core customization routines
;; Package: dlisp (Davin's version of elisp)
;; Note: The d- prefix stands for _D_avin's customizations to Emacs
;; Version: 2.0

;;; Commentary:

;; This program is part of GNU Java Training Wheels

;; This file contains definitions of functions that are used by other
;; elisp files.

;;; Limitation of Warranty

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs, see the file COPYING.  If not, see:
;;
;; <http://www.gnu.org/licenses/gpl-3.0.txt>.


;;; Install Instructions:
;; See the following URL for the latest info and a tarball:
;;
;; <http://davin.50webs.com/research/2006/mopa2e.html#diagnose>
;;
;; Then untar the tarball to a folder pointed to by the Emacs variable
;; load-path and add the following line to your ~/.emacs file.
;;
;; (require 'diagnose)

;;; Known Bugs:

;; None!

;;; Code:

(defvar prefs-advanced-user-p nil
  "Enables ESC ESC (Eval Expression), backquote, dabbrev key,
aliases, (not d-movement) and track-eol

not prefs-advanced-user-p => f5 for html indent buffer

")


(defvar prefs-calendar-online-p nil
  "Whether or not to set the calendar online")

(defvar prefs-home-emacs-p nil
  "Sets READONLY and d-log navigation"
  ;; was d-movement.el
  )

(defvar prefs-font-p t
  "Home-style fontification online")

(defvar prefs-font-libd-p nil
  "Fontification of libd online")

(defvar prefs-font-xtra-p t
  "Extra fontification online (in case not d-htmlize.el)")

(defvar prefs-lcd-emacs-p nil
  "Sets bizarre LCD colour scheme")

(defvar prefs-font-lisp++-p nil
  "Turns on Lisp++ fontification")

(defvar prefs-windows-bindings-p t
  "To load cua.el (if t) and C-o bound to dlgopen-open-files
and C-z for UNDO
")

(defvar prefs-scratch-java-online-p nil
  "Java keywords appear in the scratch buffer")

(defvar prefs-scratch-libd-online-p nil
  "LIBD keywords appear in the scratch buffer")

(defvar prefs-dlisp-keybindings-online t
  "Load dlisp keybindings (for debugging Elisp)")

(defvar prefs-safe-comands-online-p t
  "Load dlisp safe commands (for debugging Elisp)")

(defvar prefs-large-font-p t
  "Whether or not to set the large font")

;;(load "cl-macs" nil t)
;;(require 'cl-19 "cl")
(require 'cl)

(assert (fboundp 'incf))
(assert (fboundp 'cdddr))

(defmacro d-quote (&rest rest)
  )

(d-quote 1 2 3)

;;; (fboundp 'let)
;;; NOTE: bug fix for emacs 23.* lacking this function
(if (not (fboundp 'read-only-mode))
    (defun read-only-mode (&optional arg)
      "P"
      (toggle-read-only arg)))

 ;; DIAGNOSTIC VARIABLES

;;(checkpoint "d2")

(setq emacs-dialect--dosemacs-p (if (string-match "msdos" (emacs-version)) t))

(setq emacs-dialect--xemacs-p
      (and (boundp 'xemacsp)
           (if (string-match "/usr/share/xemacs-" (car (last load-path))) t)))

(setq emacs-dialect--gnuemacs-p (eq system-type 'gnu/linux))

(setq os-type--microsoft-p (if (memq system-type '(windows-nt ms-dos)) t))

(setq os-type--linux-p (eq system-type 'gnu/linux))

(setq os-type--mswindows-p
      (if (and os-type--microsoft-p
               (not os-type--linux-p)
               (not emacs-dialect--dosemacs-p)) t))

(setq os-type--windows-p os-type--mswindows-p)

(setq os-type--msdos-p
      (if emacs-dialect--dosemacs-p t))

(setq os-type--graphical-p
      (let ((answer nil))
        (save-window-excursion
          (list-colors-display)
          (set-buffer "*Colors*")
          (goto-char (point-min))
          (setq answer (re-search-forward "snow" nil t))
          (kill-buffer "*Colors*")
          (if answer t nil))))

(setq os-type--text-p
      (not os-type--graphical-p))

 ;; ESSENTIAL KEYBINDINGS

;;(checkpoint "d3")

(if (fboundp 'pc-bindings-mode) (pc-bindings-mode))

;;(when t () 'foo)

(when prefs-dlisp-keybindings-online

  (global-set-key "\C-xg"    'goto-line)
  (global-set-key "\C-x\C-g" 'goto-line)
  (global-set-key "\C-ha"    'apropos) ;; NOTE: better than command-apropos
  ;;(global-set-key "\C-ha"    'command-apropos)
  ;;(global-set-key "\C-x\C-q" 'save-buffers-kill-emacs)
  (global-unset-key "\C-x\C-q" )
  ;;(global-set-key "\C-x\C-c" (function (lambda () (interactive) (message "Press \C-x\C-q to quit") (beep) (sleep-for 1))))
  (global-unset-key "\C-x\C-c" )

  (global-set-key "\C-d" 'd-control-f)
  (global-set-key "\C-f" 'd-control-f)

  (defun d-patch-control-d ()
    (local-set-key "\C-d" 'd-control-f)
    (local-set-key "\C-f" 'd-control-f)
    )

  (add-hook 'c-mode-common-hook 'd-patch-control-d)

  (defun d-control-f (filename)
    "Fixed the bug where you try to load the same file into Emacs, instead loads the directory into dired mode."
    (interactive "FFind file/folder: ")
    ;;(if (not (boundp 'safe-expand-file-name))
    (if (string-match "/$" filename)
        (find-file (substring filename 0 -1))
      (find-file filename))
    ;;  (if (string= (safe-expand-file-name (buffer-file-name))
    ;;               (safe-expand-file-name filename))
    ;;      (dired (file-name-directory filename))
    ;;    (find-file filename)))
    )

  (global-set-key "\C-s" 'd-control-s)

  (defun d-control-s ()
    (interactive)
    (let ((table (syntax-table))
          (case-fold-search t))
      (unwind-protect
          (progn
            ;; NOTE: this line makes it work...
            (set (make-local-variable 'font-lock-syntax-table) table)
            (if (and (boundp 'c-mode-syntax-table) c-mode-syntax-table)
                (set-syntax-table c-mode-syntax-table))
            (isearch-forward))
        (set-syntax-table table))))

  (global-set-key "\M-\C-s" 'd-meta-control-s)

  (defun d-meta-control-s ()
    (interactive)
    (let ((table (syntax-table))
          (case-fold-search t))
      (unwind-protect
          (progn
            ;; NOTE: this line makes it work...
            (set (make-local-variable 'font-lock-syntax-table) table)
            (if (and (boundp 'c-mode-syntax-table) c-mode-syntax-table)
                (set-syntax-table c-mode-syntax-table))
            (isearch-forward t))
        (set-syntax-table table))))

  (global-set-key "\C-r" 'd-control-r)

  (defun d-control-r ()
    (interactive)
    (let ((table (syntax-table))
          (case-fold-search t))
      (unwind-protect
          (progn
            ;; NOTE: this line makes it work...
            (set (make-local-variable 'font-lock-syntax-table) table)
            (if (and (boundp 'c-mode-syntax-table) c-mode-syntax-table)
                (set-syntax-table c-mode-syntax-table))
            (isearch-backward))
        (set-syntax-table table))))

  (global-set-key "\M-\C-r" 'd-meta-control-r)

  (defun d-meta-control-r ()
    (interactive)
    (let ((table (syntax-table))
          (case-fold-search t))
      (unwind-protect
          (progn
            ;; NOTE: this line makes it work...
            (set (make-local-variable 'font-lock-syntax-table) table)
            (if (and (boundp 'c-mode-syntax-table) c-mode-syntax-table)
                (set-syntax-table c-mode-syntax-table))
            (isearch-backward t))
        (set-syntax-table table))))

  (let ((b (generate-new-buffer "*eraseme*")))
    ;;
    ;; NOTE: ensures that c-mode-syntax-table is bound for C-s / C-r
    ;;
    (set-buffer b)
    (c++-mode)
    (kill-buffer b)
    )

  (global-set-key "\C-w" 'really-cut)

  (defun really-cut ()
    (interactive "*")
    (if (y-or-n-p "Really cut? ")
        (call-interactively 'kill-region)))

  (defun insert-prin1 (args)
    (insert (prin1-to-string args)))

  ;; (setq x 32.456)
  (defun d-show (x)
    (kill-sexp 2)
    (insert (format " %5.2f " (/ (round (* x 100)) 100.0))))

  (defun d-dollar (x)
    (kill-sexp 2)
    (insert (format " $%5.2f " (/ (round (* x 100)) 100.0))))

  (defun d-f1 ()
    (interactive)
    (info)
    (delete-other-windows))

  (global-set-key [f1] 'd-f1)

  (defun d-meta-f1 ()
    (interactive)
    (kill-ring-save (point-min) (point-max))
    (message "Copied buffer to clipboard"))

  (global-set-key [(meta f1)] 'd-meta-f1)
  (global-set-key [(shift f1)] 'd-meta-f1)
  (global-set-key [(control f1)] 'd-meta-f1)

  (global-set-key [(f2)] 'd-f2)

  (defun d-f2 ()
    (interactive)
    (save-excursion
      (save-window-excursion
        (message "Saving some buffers")
        (save-some-buffers 'NO-QUESTIONS))))

  (global-set-key [f4] 'delete-other-windows)
  (global-set-key [f8] 'undo)

  (global-set-key [f9] 'd-f9)

  (defun d-f9 ()
    (interactive)
    (save-some-buffers 'NO-QUESTIONS)
    (execute-kbd-macro "\M-xcompile\n"))

  (global-set-key [(control f9)] 'd-shift-f9)
  (global-set-key [(meta    f9)] 'd-shift-f9)
  (global-set-key [(shift   f9)] 'd-shift-f9)

  (defun d-shift-f9 ()
    (interactive)
    (save-some-buffers 'NO-QUESTIONS)
    (call-interactively 'compile)
    )

  (assert (boundp 'prefs-advanced-user-p))
  (if prefs-advanced-user-p
      (global-set-key [(f12)] 'eval-last-sexp)
    (global-set-key [(f12)] 'electric-buffer-list))

  ;;(global-set-key [f12] 'backward-delete-char-untabify) ;; NOTE: was here

  (progn
    ;;(global-unset-key [kp-delete])
    (global-set-key [kp-delete] 1) ;;; NOTE: better than the above way to unset the key
    )

  (when prefs-advanced-user-p
    (global-set-key "\e\e" 'eval-expression)
    (global-set-key "`" 'dabbrev-expand)
    ;; for when you miss the real key
    (global-set-key "\e`" 'dabbrev-expand)
    (global-set-key "'" 'd-quote-key)
    (global-set-key "\M-/" 'dabbrev-expand)
    (global-set-key "¿" 'dabbrev-expand)

    (global-set-key "\"" 'self-insert-command)
    ;;(global-set-key "`" 'self-insert-command)

    (defun d-quote-key ()
      (interactive)
      (if (eq (char-after (1- (point))) ?')
          (progn
            (backward-delete-char 1) ;; varsity fixed:
            (insert "`"))
        (if (eq (char-after (1- (point))) ?`)
            (progn
              (backward-delete-char 1) ;; varsity fixed:
              (insert "'"))
          (self-insert-command 1))))

    )

  (if (not prefs-windows-bindings-p)
      (global-set-key [(shift   up)] 'd-shift-up))

  (global-set-key [(control up)]          'd-shift-up)
  (global-set-key [(meta    up)]          'd-shift-up)
  (global-set-key [(prior)]               'd-shift-up)

  (defun d-shift-up ()
    (interactive)
    (scroll-down 2))

  (global-set-key [(control down)]        'd-shift-down)
  (global-set-key [(meta    down)]        'd-shift-down)
  (if (not prefs-windows-bindings-p)
      (global-set-key [(shift   down)]        'd-shift-down))

  (global-set-key [(next)]                'd-shift-down)

  (defun d-shift-down ()
    (interactive)
    (scroll-up 2))

  (global-set-key [(control next)]        'scroll-up)
  (global-set-key [(meta    next)]        'end-of-defun)
  (global-set-key [(shift   next)]        'd-forward-page)

  (defun d-forward-page ()
    (interactive)
    (if (fboundp 'd-deposit-mark-if-small-movement) (d-deposit-mark-if-small-movement))
    (if (not (re-search-forward "^[ \t]*" nil t))
        (goto-char (point-max)))
    ;;(forward-page)
    (if (= (point) (point-max))
        (recenter (- (window-height) 4))
      (recenter (/ (window-height) 6))))

  (global-set-key [(control prior)]       'scroll-down)
  (global-set-key [(meta    prior)]       'beginning-of-defun)
  (global-set-key [(shift   prior)]       'd-backward-page)

  (defun d-backward-page ()
    (interactive)
    (beginning-of-line)
    (if (not (re-search-backward "^[ \t]*" nil t))
        (goto-char (point-min)))
    (skip-chars-forward " \t")
    (skip-chars-forward "")
    (if (fboundp 'd-deposit-mark-if-small-movement) (d-deposit-mark-if-small-movement))
    ;;(backward-page)
    (recenter (/ (window-height) 6)))

  (progn
    (global-set-key [(control meta prior)]  'd-backward-page)
    (global-set-key [(control meta next)]   'd-forward-page))

  (defun d-control-prior ()
    (interactive)
    (d-scroll-up (- (window-height) 2)))

  (defun d-control-next ()
    (interactive)
    (d-scroll-down (- (window-height) 2)))

  (defun d-scroll-up (amount)
    "Used by d-control-meta-prior"
    (scroll-down amount)
    (forward-line (- amount))
    (move-to-window-line (/ (window-height) 2)))

  (defun d-scroll-down (amount)
    "Used by d-control-meta-next"
    (if (/= (point) (point-max))
        (progn
          (scroll-up amount)
          (forward-line amount)))
    (move-to-window-line (/ (window-height) 2)))

  (progn
    (global-set-key [(control home)]      'beginning-of-buffer)
    (global-set-key [(control end)]       'end-of-buffer)
    (global-set-key [(meta home)]         'beginning-of-buffer)
    (global-set-key [(meta end)]          'end-of-buffer)
    (global-set-key [(shift home)]        'beginning-of-buffer)
    (global-set-key [(shift end)]         'end-of-buffer)
    )

  (global-set-key [(control right)]       'forward-sexp)
  (global-set-key [(meta    right)]       'forward-sexp)
  (global-set-key [(shift   right)]       'forward-sexp)
  (global-set-key [(control meta right)]  'forward-sexp)

  (global-set-key [(control left)]        'backward-sexp)
  (global-set-key [(meta    left)]        'backward-sexp)
  (global-set-key [(shift   left)]        'backward-sexp)
  (global-set-key [(control meta left)]   'backward-sexp)

  (global-set-key [backspace]             'backward-delete-char) ;; NOTE: not needed but good
  (global-set-key [delete]                'delete-char)          ;; NOTE: Needed sometimes as unset by default

  (global-set-key [(control backspace)]   'backward-kill-word)
  (global-set-key [(meta    backspace)]   'backward-kill-word)
  (global-set-key [(shift   backspace)]   'backward-kill-word)

  (global-set-key [(control delete)]      'kill-word)
  (global-set-key [(meta    delete)]      'kill-word)
  (global-set-key [(shift   delete)]      'kill-word)

  ;;(if prefs-advanced-user-p
  (global-set-key [insert] 'electric-buffer-list)
  ;;)

  (global-set-key [(control tab)]         'other-window)
  (global-set-key [(shift   tab)]         'other-window)

  (global-set-key [(control ?0)]          'delete-window)
  (global-set-key [(control ?1)]          'delete-other-windows)
  (global-set-key [(control ?@)]          'split-window-vertically) ;; NOTE: Only applicable to Dos Emacs
  (global-set-key [(control ?2)]          'split-window-vertically)
  (global-set-key [(control ?3)]          'split-window-horizontally)

  (global-set-key [(shift ? )] 'd-shift-space)
  (global-set-key "\M- " 'd-meta-space)    ;; NOTE: more convenient than C-u C-SPC
  ;;(global-set-key "\M- " 'pop-global-mark)

  (defun d-meta-space ()
    (interactive)
    (message "d-meta-space")
    ;;(set-mark-command 1)
    (pop-mark)
    (recenter))

  (defun d-shift-space ()
    (interactive)
    (push-mark (point) 'NO-MSG)
    (message "d-shift-space")
    (recenter))

  (when prefs-advanced-user-p
    (defalias   'dtp 'describe-text-properties)
    ;;(defalias 'lta 'list-text-properties-at)  ;; version 21.3
    (defalias   'lta 'describe-text-properties) ;; version 22.1
    (defalias   'elm 'emacs-lisp-mode)
    (defalias   'flm 'font-lock-mode)
    (defalias   'qrr 'query-replace-regexp)

    (defalias   'eb  'eval-buffer)
    (defalias   'hff 'hexl-find-file)
    )

  (defalias 'buffer-substring-properties 'buffer-substring)

  (defun tde ()
    (interactive)
    (message "tde")
    (if debug-on-error
        (setq debug-on-error nil)
      (setq debug-on-error t)))

  )

(progn
  (global-set-key [(shift ? )] 'd-pop-mark)
  (global-set-key [(s ?\s- )] 'd-push-mark)

  (defun d-pop-mark ()
    (interactive)
    (pop-mark))

  (defun d-push-mark ()
  (interactive)
  (push-mark))
  )


 ;; SAFE COMMANDS

;;; (d-ding)
(defun d-ding ()
  (let ((visible-bell t))
    (beep))
  )

;;(checkpoint "d4")
;; (d-nbeeps 3 "hello joe")
(defun d-nbeeps (n &rest msg)
  (let ((string (concat "*** BEEPS: " (apply 'format msg))))
    (decf n)
    (decf n)
    (d-ding)
    (message string)
    (sleep-for 1)
    (while (> n 0)
      (d-ding)
      (sleep-for 1)
      (decf n))
    (ding t)))

(defun d-beeps (&rest msg)
  "Defined by me"
  (let ((string       (concat "*** BEEPS: " (apply 'format msg) ))
        (d-message-on t))
    (d-ding)
    (message string)
    (sleep-for 1)
    (d-ding)
    (sleep-for 1)
    (d-ding)))

(defun d-beep (&rest msg)
  "Defined by me"
  (let ((string       (concat "*** BEEP: " (apply 'format msg) ))
        (d-message-on t))
    (d-ding)
    (message string)
    (sleep-for 1)
    ))

;; (safe-command (forward-sexp 1))
;; (safe-command (message "foo"))
;; (safe-command (safe-load-file "~/dlisp/imported-stuff/zzdotemacs.el"))
;; (safe-command (setq cmd (make-directory "/media/www/G16GB/home/bak/" 'parents)))
;; (safe-command (setq cmd (make-directory "/media/www/BLU16GB/home/bak/" 'parents)))
(defmacro safe-command (cmd &optional quietp)
  ;;(debug "Tuesday")
  `(condition-case err
       ,cmd
     (error
      (if (not ,quietp)
          (d-beeps "Command: %s failed, reason = %s, current-buffer = %s"
                   (quote ,cmd)
                   err
                   (buffer-name)
                   )))))

;;; (prin1-to-string (setq foo 'bar))

(defun safe-load-file (filename)
  (let ((cmd (list 'load-file filename)))
    (safe-command (eval cmd))))

;; (safe-load-file "~/dlisp/imported-stuff/tabbar.el")
;; (safe-command (safe-load-file "~/dlisp/imported-stuff/tabbar.el"))

(defun safe-load-library (filename)
  (let ((cmd (list 'load-library filename)))
    (safe-command (eval cmd))))

(defun safe-require (feature)
  (condition-case err
      (progn
        ;;(message "*** About to provide feature %s" feature)
        (require feature)
        ;;(message "*** Feature %s provided" feature)
        )
    (error (d-beeps "*** require %s failed, error = %s" feature err))))

;;(defun safe-require (feature)
;;  (require feature))

(defun safe-find-file (file)
  (if (file-exists-p file)
      (if (fboundp 'd-find-file)
          (d-find-file file)
        (find-file file))))

(defun safe-downcase (string)
  (if string
      (downcase string)
    nil))

(defun safe-getenv (var)
  "Useful as the second argument to string-match"
  (setq var (getenv var))
  (if (eq var nil) "" var))

;;;
;;; NOTE: standard notation follows...
;;;
;;; File/directory spec is  (setq dir-1 "d:/home") or (setq dir-1 "d:") (note no trailing slash)
;;; File access is          (concat dir-1 "/" file-1)
;;; Relative directory spec (setq rel-1 "abc/def")
;;; Therefore joiner is     (concat dir-1 "/" rel-1)
;;; Shell script joiner is  $ABC/$DEF or $ABC/foo (note $ABCfoo is an error)
;;; Makefile joiner is      $$ABC/$$DEF or $$ABC/foo (note $$ABCfoo is an error)
;;; Makefile joiner is      $(ABC)/$(DEF) or $(ABC)/foo (note $(ABC)foo works without error)
;;;
;;; BUG: d: denotes the current directory in the d drive in some cases (outside of Emacs)
;;;
;;; (safe-expand-file-name nil)
;;; (safe-expand-file-name "d:/")
;;; (safe-expand-file-name (setq file "d:/home/"))
;;; (safe-expand-file-name "d:/home")
;;; (safe-expand-file-name "d:/home/.emacs")
;;; (safe-expand-file-name "d:/home//text/..")
;;;
;;; NOTE: never returns a trailing slash
;;;
;;; (setenv "HOME" "d:/")
;;; (safe-expand-file-name "d:/")
;;; (safe-expand-file-name "d:/frog")
;;; (safe-expand-file-name "d:/FROG")
;;; (safe-expand-file-name "~/.emacs")
;;; (safe-expand-file-name "~/dlisp")
;;;
(defun safe-expand-file-name (filename)
  "Replaces ~/ with expanded file name"

  (save-match-data
    (setq filename (if filename (expand-file-name filename)))
    ;;
    ;; NOTE: removes trailing slash
    ;;
    (if (and filename (string-match "/$" filename))
        (setq filename (substring filename 0 -1)))

    filename))

;;;
;;; (safe-compress-file-name nil)
;;; (safe-compress-file-name "d:/")
;;; (safe-compress-file-name "d:/home")
;;; (safe-compress-file-name (setq filename "d:\\home"))
;;; (safe-compress-file-name (setq filename "d:\\home\\"))
;;; (safe-compress-file-name "d:\\home\\fddf")
;;; (safe-compress-file-name "d://home//frog")
;;; (safe-compress-file-name "d:/HOME/fddf")
;;; (safe-compress-file-name "d:/home/fddf/")
;;; (safe-compress-file-name "d:/home//fddf")
;;; (safe-compress-file-name "d:/fred/fddf")
;;;
;;; NOTE: never returns a trailing slash
;;;
;;; (setenv "HOME" "d:/")
;;; (safe-compress-file-name "d:/")
;;; (safe-compress-file-name "d:/frog")
;;; (safe-compress-file-name "d:/home//text/..")
;;; (safe-compress-file-name "/home/www/text")
;;; (safe-compress-file-name "/home/davin/text")
;;;
(defun safe-compress-file-name (filename)
  "Replaces expanded ~/ expression with ~/"

  (save-match-data

    (assert (getenv "HOME"))
    (assert (file-directory-p (getenv "HOME")))

    (let* ((filename (safe-expand-file-name filename))
           (new-home (safe-expand-file-name (getenv "HOME"))))

      ;;(debug)

      (progn
        ;;
        ;; NOTE: adds trailing slash
        ;;
        (if (and filename (file-directory-p filename) (not (string-match "/$" filename)))
            (setq filename (concat filename "/")))
        (if (and new-home (not (string-match "/$" new-home)))
            (setq new-home (concat new-home "/"))))

      ;;(debug)
      ;; (safe-compress-file-name "d:/home")
      ;; (setq new-home "d:/home/")
      ;; (setq filename "d:/home/")

      ;;
      ;; NOTE: replaces HOME folder with ~/
      ;;
      (if (and filename (string-match (concat "^" new-home "\\(.*$\\)") filename))
          (setq filename (concat "~/" (substring filename (match-beginning 1) (match-end 1)))))

      (progn
        ;;
        ;; NOTE: removes trailing slash
        ;;
        (when (and filename (string-match "/$" filename))
          ;;(d-foo)
          (setq filename (substring filename 0 -1)))
        )

      filename)))

;; (setq to "~/src/davin/research/2004")
;; (setq from "~/four")
;;(safe-make-link "~/src/davin_pearson/research/2004" "~/four")
(defun safe-make-link (to from)
  ;;(debug "smeg")
  (assert (stringp to))
  (assert (stringp from))
  (if (and (file-exists-p to)
           (not (file-exists-p (concat from ".lnk"))))
      (shell-command (concat "ln -s " (expand-file-name to) " " (expand-file-name from)))))

(defun safe-window-restore ()
  (if (fboundp 'w32-send-sys-command)
      (w32-send-sys-command 61728)
    )
  )

(defun safe-window-maximise ()
  (if (fboundp 'w32-send-sys-command)
      (w32-send-sys-command 61488)
    )
  )

(global-set-key [(control l)] 'd-recenter)

(d-quote if (and (boundp 'prefs-large-font-p)
                 prefs-large-font-p
                 os-type--microsoft-p)
         (progn
           ;;(d-beeps "Loading courier font")
           (set-default-font "-*-courier new-normal-r-*-*-22-*-*-*-c-*-fontset-standard")
           )
         (set-default-font "-*-Courier New-normal-r-*-*-16-*-*-*-c-*-iso8859-1"))

(defun d-recenter ()
  (interactive)
  (recenter)
  (progn
    (safe-window-restore)
    (safe-window-maximise)
    )
  )

(global-set-key "\C-b" 'd-kill-buffer)

(defun kill-buffer-no-questions (&optional buf)
  (interactive)
  (set-buffer-modified-p nil)
  (kill-buffer buf)
  )

(defun d-kill-buffer ()
  (interactive)
  (unwind-protect
      (if (or (string= (buffer-name) "*Messages*")
              (string= (buffer-name) "*scratch*")
              (string= (buffer-name) "*compilation*")
              (eq major-mode 'dired-mode))
          (bury-buffer)
        (kill-buffer nil))
    ;;(if (fboundp 'd-speedbar--kill-speedbar-buffers)
    ;;    (run-with-timer 1 nil 'd-speedbar--kill-speedbar-buffer))
    ))

(d-quote defun d-kill-buffer ()
  ;;(bury-buffer)
  )

(d-quote defun d-speedbar--kill-speedbar-buffers ()
  ;;(message "foo")
  (sit-for 1)
  ;;(d-walk-frames
  ;;(if (string= (buffer-name (current-buffer)) (cdar ptr-walk))
  ;;     (kill-buffer (cdar ptr-walk)))
  )

(define-key
  global-map
  [tool-bar kill-buffer]
  '(menu-item
    "Maybe Kill Buffer"
    d-kill-buffer
    :enable 1
    :visible 1
    :help "Maybe Kill Buffer"
    :image (image :type xpm :file "../etc/images/close.xpm")
    ))

 ;; DLISP COMMANDS

;;(checkpoint "d5")

(defun d-cadar (list)
  (car (cdr (car list))))

(defun d-current-line-as-string ()
  (buffer-substring-no-properties (point-at-bol)
                                  (point-at-eol)))

(defun d-current-line-width ()
  (length (d-current-line-as-string)))

(defun read-ready (str)
  (and (stringp str) (not (string= "" str))))

;; (setq str "")
(defun read-str (str)
  (assert (read-ready str))
  (read str))

;; (setq str1 "abc")
;; (setq str2 "Abc")
;; (string=-ignore-case str1 str2)
(defun string=-ignore-case (str1 str2)
  (string= (upcase str1) (upcase str2)))

(defun string<-ignore-case (str1 str2)
  (string< (upcase str1) (upcase str2)))

(defun d-current-buffer-to-lines ()
  (let ((answer nil))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (setq answer (cons (d-current-line-as-string) answer))
        (forward-line 1)))
    (reverse answer)))

(defun d-sort-buffer ()
  (interactive)
  (let ((list))
    (setq list (d-current-buffer-to-lines))
    (setq list (stable-sort list 'string<))
    (erase-buffer)
    (let ((ptr list))
      (while ptr
        ;;(debug)
        (insert (car ptr) "\n")
        (setq ptr (cdr ptr))))))

;;;
;;; (find-file "~/dlisp/keys.el")
;;; (d-currently-editing-file "d:\\home\\dlisp\\keys.el")
;;; (d-currently-editing-file "d:\\home\\not\\editing.el")
;;; (d-currently-editing-file "d:\\HOME\\DLISP\\DIAGNOSE.EL")
;;;
(defun d-currently-editing-file (filename)
  "If not editing that filename, retures nil
otherwise returns the buffer object corresponding to the given filename."
  (setq filename (safe-expand-file-name filename))

  (if os-type--microsoft-p
      (setq filename (safe-downcase filename)))

  (let* ((list  (buffer-list))
         (ptr   list)
         (found nil))

    (while (and ptr (not found))
      (let ((curname (buffer-file-name (car ptr))))
        (setq curname (safe-expand-file-name curname))
        (if os-type--microsoft-p (setq curname (safe-downcase curname)))
        (if (string= curname filename)
            (setq found (car ptr))))
      (setq ptr (cdr ptr)))
    found))

(setq d-emacs-start-time (current-time))

(defun d-delete-line ()
  (delete-region (point-at-bol) (point-at-eol))
  (if (looking-at "\n")
      (delete-char 1))
  )

(defun d-delta-looking-at (regexp delta)
  (save-excursion
    (forward-char delta)
    (looking-at regexp)))

(defun d-font-lock-add-begin (keywords)
  (if (fboundp 'font-lock-add-keywords)
      (font-lock-add-keywords nil keywords nil)
    (setq font-lock-keywords
          (append
           keywords
           font-lock-keywords))))

(defun d-font-lock-add-end (keywords)
  (if (fboundp 'font-lock-add-keywords)
      (font-lock-add-keywords nil keywords 'end)
    (setq font-lock-keywords
          (append
           font-lock-keywords
           keywords))))

(defun d-font-lock-remove-keywords ()
  (if (fboundp 'font-lock-add-keywords)
      (font-lock-add-keywords nil nil 'set)
    (setq font-lock-keywords nil)))

(defun d-foo (&optional who-cares)
  "Useful function for debugging"
  (interactive)
  (message "###############")
  (sleep-for 0.1)
  (message "               ")
  (sleep-for 0.1)
  (message "###############")
  (sleep-for 0.1)
  (message "               ")
  (sleep-for 0.1)
  (message "###############")
  (sleep-for 0.1)
  (message "               ")
  nil
  )

(defun d-insert-prin1 (x)
  (insert (prin1-to-string x) "\n"))

(defun d-is-first-line ()
  "Faster than (eq (d-what-line) 1)"
  (save-excursion
    (beginning-of-line)
    (bobp)))

(defun d-is-last-line ()
  (save-excursion
    (end-of-line)
    (eobp)))

;;; (get-buffer-window)
;;; (d-is-buffer-visible (current-buffer))
(defun d-is-buffer-visible (buffer)
  (and buffer (get-buffer-window buffer)))

;;; (d-kill-adjacent-duplicates '("abc" "abc" "abc" "def" "abc" "abc"))
;;; (d-kill-adjacent-duplicates '("abc" "abc" "abc" "abc" "abc" "abc" "def" "abc" "abc"))
;;; (d-kill-adjacent-duplicates '("abc" "abc" "abc" "abc" "abc" "abc" "def" "abc" "abc"))
(defun d-kill-adjacent-duplicates (list)
  ;;(setq list '("abc" "abc" "def" "abc"))
  (let ((ptr list))
    (while ptr
      (while (string= (car ptr) (cadr ptr))
        ;;(debug)
        (setcar ptr (cadr ptr))
        (setcdr ptr (cddr ptr)))
      (setq ptr (cdr ptr)))
    list))

;;(defun d-kill-adjacent-duplicates (list)
;;  (d-kill-adjacent-duplicates--inner (d-kill-adjacent-duplicates--inner list)))

;;;
;;; (d-last-string-match "src" "~/hairy-lemon/src/davin/webdesign/tutorial-4/src/site/index.hts") ;; yes
;;; (d-last-string-match "src" "~/hairy-lemon/src/davin/webdesign/tutorial-4/not/site/index.hts") ;; yes
;;; (d-last-string-match "src" "~/hairy-lemon/not/davin/webdesign/tutorial-4/not/site/index.hts") ;; no
;;;
(defun d-last-string-match (regexp string)
  (let ((i    (length string))
        (done nil))
    (while (and (>= i 0) (not done))
      (if (string-match regexp string i)
          (setq done t))
      (decf i))
    done))

;;;
;;; COOL:
;;;
(defun d-splat (old-text new-text)
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (while (search-forward old-text nil t)
        (replace-match new-text nil nil)))))

;;;
;;; COOL:
;;;
(defun d-splat-re (old-text new-text)
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward old-text nil t)
        (replace-match new-text nil nil)))))

;;;
;;; (d-trim-left "\r\t abc \t\r\n  abc def  ")
;;;
(defun d-trim-left-old (s)
  (progn
    (while (and (setq ch (aref s 0))
                (or (eq ch ?\ )
                    (eq ch ?\t)
                    (eq ch ?\r)
                    (eq ch ?\n)))
      (setq s (substring s 1)))
    s)
  )

;; (d-trim-left "        abc  def        ")
;; (d-trim-left " ")
(defun d-trim-left (s)
  (let (i len ch)
    (setq i 0)
    (setq len (length s))
    (while (and (< i len)
                (setq ch (aref s i))
                (or (eq ch ?\ )
                    (eq ch ?\t)
                    (eq ch ?\r)
                    (eq ch ?\n)))
      (incf i))
    (setq s (substring s i)))
  s)

;;;
;;; (d-trim-right "abc def \r\t abc \r\t\n def \r\n")
;;; (d-trim-right s)
(defun d-trim-right-old (s)
  (progn
    (setq i (1- (length s)))
    (while (and (setq ch (aref s i))
                (or (eq ch ?\ )
                    (eq ch ?\t)
                    (eq ch ?\r)
                    (eq ch ?\n)))
      (setq s (substring s 0 i))
      (decf i))
    s)
  )

;; (d-trim-right "abc        def \n\n\n")
;; (d-trim-right " ")
(defun d-trim-right (s)
  (let (i ch)
    (setq i (1- (length s)))
    (while (and (>= i 0)
                (setq ch (aref s i))
                (or (eq ch ?\ )
                    (eq ch ?\t)
                    (eq ch ?\r)
                    (eq ch ?\n)))
      ;;(setq s (substring s 0 i))
      (decf i))
    (setq s (substring s 0 (1+ i)))
    )
  )

;;;
;;; (d-trim-string (setq s "\r\t\n  abc def \n sexy rexy \n"))
;;; (d-trim-left "")
;;; (d-trim-right "")
(defun d-trim-string (s)
  (if (or (not s) (string= s ""))
      ""
    (if s (d-trim-left (d-trim-right s)))))

;;(defun d-what-line ()
;;  (read (substring (what-line) 5)))

(defun d-what-line ()
  "Different from what-line in that it returns the result rather
than printing it"
  (save-excursion
    (beginning-of-line)
    (1+ (count-lines 1 (point)))))

;;(global-set-key [kp-enter] 'd-random-play-emacs-midi)


 ;; VARIABLES


;;(checkpoint "d6")

;;
;; NOTE: these variable appear to have no effect
;;
(d-quote
  (setq blink-cursor-interval 0.5)
  (setq blink-cursor-delay 0.5)
  (run-with-idle-timer 0.1 t 'blink-cursor-start))

(setq kill-whole-line t)
;;(setq eol-mnemonic-unix "(Unix)")
(setq eol-mnemonic-dos "(Dos)")
(if prefs-advanced-user-p
    (setq track-eol t)
  (setq track-eol nil))
(setq compile-command "make ")
(setq message-log-max 10000)
(put 'eval-expression  'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'set-goal-column  'disabled nil)
(setq enable-local-eval t)

(setq backup-inhibited t)   ;; NOTE: disables ~ style backups
(setq auto-save-timeout 30) ;; NOTE: #*# style backups are online

(setq-default c-basic-offset 3)
(setq-default
 transient-mark-mode               nil
 indent-tabs-mode                  nil   ;;; never put tabs in d- code
 parse-sexp-ignore-comments        t
 comment-column                    40
 completion-ignore-case            t     ;;; Good for Win95 mode.
 case-fold-search                  nil
 c-basic-offset                    3
 truncate-lines                    nil
 )

(setq
 gc-cons-threshold                 2000000
 visible-bell                      t
 same-window-buffer-names          nil
 compilation-window-height         15
 line-number-mode                  t
 require-final-newline             t
 search-highlight                  t
 compilation-window-height         10
 compilation-ask-about-save        nil
 next-line-add-newlines            nil ;; Doesn't add newlines at end
 kill-whole-line                   t   ;; Makes C-k more convenient.
 next-screen-context-lines         1   ;; Overlap with PGUP/PGDN
 compile-command                   "make "
 default-major-mode                'fundamental-mode
 three-step-help                   t
 apropos-do-all                    t   ;; more extensive online help
 enable-recursive-minibuffers      nil ;; advanced customisation feature...
 column-number-mode                t
 diary-file                        (expand-file-name "~/.diary")
 eval-expression-debug-on-error    nil
)

(setq undo-limit (* 1000 1000))
(progn
  ;;
  ;; NOTE: this command is obsolete
  ;;
  ;;(hscroll-global-mode)
  ;;(setq hscroll-mode-name nil)
  )
(setq-default tab-width 8)
(setq delete-old-versions t)


 ;; HISTORY VARIABLES

;;(checkpoint "d7")

(if (or (not (boundp 'compile-history))
        (not compile-history))
    (setq compile-history '("make ")))

(setq file-name-history (cons (if emacs-dialect--gnuemacs-p
                                  (concat (getenv "HOME") "/bak/")
                                "~/bak/") file-name-history))

(if (string= (user-real-login-name) "root")
    (setq file-name-history (cons "~davin/" file-name-history)))

(if emacs-dialect--gnuemacs-p
    (setq file-name-history (cons "~/" file-name-history)))

(if (boundp 'safe-compress-file-name)
    (setq file-name-history (cons (concat (safe-compress-file-name "~/dlisp") "/") file-name-history)))

(when (and emacs-dialect--gnuemacs-p os-type--microsoft-p)
  (setq file-name-history (cons "c:/" file-name-history))
  ;;(setq file-name-history (cons (concat (safe-compress-file-name (getenv "HOME")) "/") file-name-history))
  )

;;(if os-type--linux-p (setq file-name-history (cons "~/src/davin/research/2004/" file-name-history)))

(when emacs-dialect--gnuemacs-p
  ;;(setq extended-command-history (cons "eval-buffer" extended-command-history))
  (setq extended-command-history (cons "meal-timer" extended-command-history))4
  (setq extended-command-history (cons "bak" extended-command-history))
  (setq extended-command-history (cons "grep" extended-command-history))
  (setq dired-shell-command-history
	(if (boundp 'dired-shell-command-history)
	    (cons "du -h" dired-shell-command-history)
	  '("du -h")))
  )

(when emacs-dialect--xemacs-p
  ;;(setq read-command-history (cons "eval-buffer" read-command-history))
  (setq read-command-history (cons "bak" read-command-history))
  (setq read-command-history (cons "grep" read-command-history))
  )

;;;
;;; NOTE: mouse wheel in XEmacs
;;;
(if emacs-dialect--xemacs-p
    (when window-system
      ;; enable wheelmouse support by default
      (mwheel-install)
      (setq mouse-wheel-scroll-amount '(2 . 1))
      ;; use extended compound-text coding for X clipboard
      (set-selection-coding-system 'compound-text-with-extensions)
      (set-scroll-bar-mode 'right)
      (set-frame-name (concat "Gnu Emacs " (capitalize (user-real-login-name))))))

 ;; LINKS and CRUD

;;(checkpoint "d8")

(defun d-make-links ()
  (when os-type--microsoft-p
    ;;(d-make-links--year "1999")
    ;;(d-make-links--year "2003")
    ;;(d-make-links--year "2004")
    ;;(d-make-links--year "2005")
    ;;(safe-make-link (concat webdata  "/src/davin/") "~/davin")
    ;;(safe-make-link (concat "~/zallegro/2004/Tritus-II/")    "~/tritus")
    ;;(safe-make-link (concat "~/zallegro/2005/dd3/")          "~/dd3")
    ;;(safe-make-link (concat "~/zallegro/2005/tests/")        "~/tests")
    )

  (when os-type--linux-p
    ;;(safe-make-link "/usr/share/emacs/site-lisp/dlisp/" "~/dlisp")
    ;;(safe-make-link "/bak-dos"  "~/bak-dos")
    ;;(safe-make-link "/bak-unix" "~/bak-unix")
    ;;(safe-make-link "~davin/src/davin/research/2003" "~/3")
    ;;(safe-make-link "~davin/src/davin/research/2004" "~/4")
    ;;(if (string= (user-real-login-name) "root")
    ;;    (safe-make-link "~davin/web" "~/web"))
    ;;(safe-make-link "/mnt/cdrom" "~/cdrom")
    )
  )

(d-make-links)

(if (not (assoc 'debug-on-error minor-mode-alist))
    (setq minor-mode-alist (cons '(debug-on-error " DEBUG") minor-mode-alist)))

(when prefs-home-emacs-p
  ;;(setenv "WEBDATA" "c:/home/hairy-lemon")
  (setq webdata "~/hairy-lemon"))

(global-font-lock-mode 1)
(add-hook 'text-mode-hook 'turn-on-font-lock)

(recenter)

;;(checkpoint "d9")
;;(checkpoint "d10")

;;
;; NOTE: needed for sensible bindings of CAPS LOCK key
;;
(defun d-xmodmap ()
  (interactive)
  (save-window-excursion
    (shell-command (concat "xmodmap ~www/dlisp/caps/xmodmaprc"))
    (if (get-buffer "*Shell Command Output*")
        (kill-buffer "*Shell Command Output*"))
    (message "Ran xmodmap ~www/dlisp/caps/xmodmaprc")
    ;;(sit-for 1)
    ))

;;(require 'emergency-bindings)
;;(d-emergency-set-load-path)

(when (not os-type--text-p)
  (load-file (concat (car load-path) "imported-stuff/tabbar.el"))
  (tabbar-mode 1)
  )

;;(message "*** loaded diagnose.el")

(setq file-name-history (append '("~/" "~/dlisp/" "~/lisp++-projects/") file-name-history))

;;; (setq file "~/dlisp/d-flock.el")
(d-quote defadvice load-file (around dlisp activate)
  (when (string-match "\\.\\(el\\|elc\\)$" file)
    (setq name-sans-extension (substring file 0 (match-beginning 0)))
    (setq name-plus-el        (concat name-sans-extension ".el" ))
    (setq name-plus-elc       (concat name-sans-extension ".elc"))
    (setq date-el  (nth 5 (file-attributes name-plus-el )))
    (setq date-elc (nth 5 (file-attributes name-plus-elc)))
    (setq time-el  (+ (* 65536.0 (or (car date-el ) 1)) (or (cadr date-el ) 0)))
    (setq time-elc (+ (* 65536.0 (or (car date-elc) 1)) (or (cadr date-elc) 0)))
    (if (> time-el time-elc)
        (progn
          (message "loaded file=%s" name-plus-el)
          (setq file name-plus-el)
          ad-do-it)
      (message "loaded file=%s" name-plus-elc)
      (setq file name-plus-elc)
      ad-do-it)))

(message "Provided feature diagnose")
(provide 'diagnose)
;;; diagnose.el ends here
