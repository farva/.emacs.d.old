
;;; cfm.el --- Displays the current function or method on the mode-line

;; Copyright (C) 2006-2011 Davin Pearson

;; Author/Maintainer: Davin Max Pearson <http://davin.50webs.com>
;; Keywords: Current Function method C++, Lisp++, Lisp, C, Java
;; Version: 1.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;; This code causes the current function Elisp/C/C++ or method
;; (Java/C++) to be shown in the mode line.

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
;; <http://davin.50webs.com/research/2006/mopa2e.html#cfm>
;;
;; Then untar the tarball to a folder pointed to by the Emacs variable
;; load-path and add the following line to your ~/.emacs file.
;;
;; (require 'cfm)

;;; Known Bugs:

;; None so far!

;;; Code:

(require 'diagnose)
(require 'jtw-build-jtw)

;;(global-set-key [kp-enter] 'cfm--announce)
;;(global-set-key [(shift return)]   'cfm--announce)
;;(global-set-key [(meta return)]    'cfm--announce)

;;(global-set-key [(control return)] nil)
;;(global-set-key [(shift return)]   nil)
;;(global-set-key [(meta return)]    nil))

;;(make-local-variable 'cfm--class::method)
(make-variable-buffer-local 'cfm--class::method)

;; NOTE: this works in lisp-pretty-print.exe
;;(d-quote ?* ?+ ?a ? ?\ ?\n)

;;; (aref "abc" 0)
;;; (setq cfm--class::method " Foo::bar")
;;; (setq cfm--class::method nil)
(defun cfm--announce ()
  (interactive)
  (cfm--set)
  (cond
   ((not cfm--class::method)
    (message "Class::method = nil"))
   ((eq (aref cfm--class::method 0) ? )
    (message "Class::method =%s" cfm--class::method))
   (t
    (message "Class::method = %s" cfm--class::method)))
  ;;(redraw-frame (car (frame-list)))
  )

;;;
;;; FIXME: get current method name......
;;;
;;; BUGGER: errors don't get announced by idle timer
;;;

(progn
  (kill-local-variable 'cfm--new)
  (setq-default cfm--new "")
  )

;; (cfm--get-class::method 0)
(defun cfm--outer-get-namespace::class::method ()
  (let* ((namespace (cfm--get-namespace))
         (class     (cfm--get-class (if namespace 1 0)))
         (result    nil)
         (method    nil))
    (if class
        (progn
          (setq method (or (nth 0 (cfm--get-method (if namespace 2 1))) "<No Method>"))
          ;;(debug "Roger Ramjet")
          (setq result (concat (if namespace (concat namespace "::") "") class "::" method)))
      ;;(debug "Amber Dempsey")
      (setq result (concat "::" (if namespace (concat namespace "::") "")
                           (cfm--get-class::method (if namespace 1 0)))))
    ;;(debug "Dire Straits: My Parties")
    result
    ))

(defun cfm--set ()
  (interactive)
  (let (d-message-on)
    (setq d-message-on t)
    ;;(sit-and-message "beg of cfm--set")
    ;;(setq d-message-on nil)
    (save-match-data
      (save-excursion
        (cond
         ((or (eq major-mode 'c-mode)
              (eq major-mode 'c++-mode)
              (eq major-mode 'java-mode))
          (if (and (boundp 'lisp++) lisp++)
              (let ((class (cfm--get-lisp++-class)))
                (if class
                    (let ((method (or (cfm--get-lisp++-method) "<No Method>")))
                      (setq cfm--class::method (concat " " class "::" method)))
                  (setq cfm--class::method (concat " ::" (cfm--get-lisp++-function))))
                (force-mode-line-update) ;;; better than updating the entire frame...
                )
            (setq cfm--class::method (concat " " (cfm--outer-get-namespace::class::method)))
            (force-mode-line-update) ;;; better than updating the entire frame...
            ))
         ((eq major-mode 'emacs-lisp-mode)
          ;;(sit-and-message "elm1")
          (setq cfm--class::method (concat " " (cfm--get-defun)))
          (force-mode-line-update)
          ;;(sit-and-message "elm2")
          )
         ((eq major-mode 'php-mode)
          (setq cfm--class::method (concat " " (car (cfm--get-php-function))))
          (force-mode-line-update))
         ((eq major-mode 'compilation-mode)
          (setq cfm--class::method (concat " " (cfm--get-compilation-strobe)))
          (force-mode-line-update))
         (t
          (setq cfm--class::method nil)))
        ))
    (setq d-message-on t)
    ;;(sit-and-message "end of cfm--set")
    ))

(defun cfm--get-defun ()
  ;;(message "foo")
  (save-excursion
    ;;(debug)
    (let ((p (point)) (r nil) (function nil) str)
      (setq str "^(\\(defun\\|defmacro\\|defadvice\\) +\\([-a-zA-Z0-9_+<>/=:!]+\\)[ \t]*\\(([^()]*)\\)")
      (cond
       ((save-excursion
          (beginning-of-line)
          (looking-at str))
        (setq function (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
        function)
       ((re-search-backward str nil t)
        (setq function (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
        (if (not (looking-at "("))
            (re-search-backward "(" nil t))
        (condition-case err
            (forward-sexp 1)
          (error nil))
        ;;(when (> (point) p)
        ;;(setq r (concat " " function))
        ;;(set-text-properties 0 (length r) 'bg:yellow r)
        (d-trim-string function))))))

(defvar cfm--is-on t
  "Set this variable to nil to disable the display of the current
function/method in the mode line.  This can be useful if
d-speedbar has been activated.")

(when cfm--is-on
  (setq cfm--timer-1 (run-with-idle-timer 2.0 t 'cfm--set))
  ;;(setq cfm--timer-2 (run-with-idle-timer 1.0 t 'cfm--smeg))
  )

(defun cfm--cancel-timers ()
  (cancel-timer cfm--timer-1)
  ;;(cancel-timer cfm--timer-2)
  )

(defun cfm--inside (orig i)
  (block nil
    (let (str p)
      (setq p (point))
      (save-excursion
        (save-match-data
          (setq str (concat "^" (make-string (* c-basic-offset i) ? ) "{"))
          (when (save-excursion
                  (forward-line 1)
                  (beginning-of-line)
                  (looking-at str))
            (skip-chars-forward " \t\r\n")
            (assert 'living-in-the-city (looking-at "{"))
            (condition-case nil
                (forward-sexp 1)
              (error nil))
            (if (> (point) orig)
                (return t)
              (when (save-excursion
                      (beginning-of-line)
                      (looking-at str))
                (skip-chars-forward " \t\r\n")
                (assert (and 'too-high (looking-at "{")))
                (condition-case nil
                    (forward-sexp 1)
                  (error nil))
                (if (> (point) orig)
                    (return t)
                  ;; -------------------------------------------------
                  (while (warn--re-search-backward--no-strings-no-comments str nil t)
                    (save-excursion
                      (skip-chars-forward " \t\r\n")
                      (assert (and 'superstition (looking-at "{")))
                      (condition-case nil
                          (forward-sexp 1)
                        (error nil))
                      (if (> (point) orig) (return t))))
                  )
                )
              )
            )
          )
        )
      (goto-char p)
      )))

(defun cfm--get-namespace ()
  (let (namespace p)
    (save-excursion
      (setq p (point))
      (beginning-of-line)
      (if (looking-at "^namespace \\([a-zA-Z0-9_]+\\)")
          (setq namespace (buffer-substring-no-properties
                           (match-beginning 1)
                           (match-end 1)))
        (when (re-search-backward "^namespace \\([a-zA-Z0-9_]+\\)" nil t)
          (setq namespace (buffer-substring-no-properties
                           (match-beginning 1)
                           (match-end 1)))
          (forward-line 1)
          (beginning-of-line)
          (skip-chars-forward " \t\r\n")
          (when (looking-at "{")
            (condition-case nil
                (forward-sexp)
              (error nil))
            (if (> (point) p)
                namespace)))))))

;; (cfm--get-class (setq i 0))
(defun cfm--get-class (i)
  (block nil
    (let ((case-fold-search nil)
          (str              nil)
          (orig             nil)
          (class            nil))
      (save-excursion
        (setq orig (point))
        (setq str (concat "^"
                          (make-string (* i c-basic-offset) ? )
                          "\\([A-Za-z]+[ \t]+\\)*\\(class\\|interface\\)[ \t]"))
        (if (save-excursion
              (beginning-of-line)
              (looking-at str))
            (progn
              (beginning-of-line)
              (assert (and 1 (re-search-forward "\\<\\(class\\|interface\\)\\>" (point-at-eol) t)))
              (skip-chars-forward " \t")
              (setq class (buffer-substring-no-properties (point) (save-excursion
                                                                    (skip-chars-forward "A-Za-z0-9_")
                                                                    (point))))

              ;;(error "smeg")
              (beginning-of-line)
              (forward-line 1)
              (skip-chars-forward " \t")
              ;;(if debug-on-error (debug "Hot Potatoes"))
              (if (and (looking-at "{")
                       (cfm--inside orig i))
                  (return class)))
          ;;(debug "Jesus loves you more than you will know")
          (goto-char orig)
          (when (re-search-backward str nil t)
            (beginning-of-line)
            (assert (and 1 (re-search-forward "\\<\\(class\\|interface\\)\\>" (point-at-eol) t)))
            (skip-chars-forward " \t")
            (setq class (buffer-substring-no-properties (point) (save-excursion
                                                                  (skip-chars-forward "A-Za-z0-9_")
                                                                  (point))))

            ;;(error "smeg")
            (beginning-of-line)
            (forward-line 1)
            (skip-chars-forward " \t")
            ;;(if debug-on-error (debug "Hot Potatoes"))
            (if (and (looking-at "{")
                     (cfm--inside orig i))
                (d-trim-string class)
              ""))) ;; END IF!
        ))))

(defun cfm--get-lisp++-class ()
  (save-excursion
    (let (class p)
      (setq p (point))
      (when (re-search-backward "^(cclass \\([a-zA-Z0-9_]*\\)" nil t)
        (setq class (buffer-substring-no-properties
                     (match-beginning 1)
                     (match-end 1)))
        (beginning-of-line)
        (forward-sexp 1)
        (if (> (point) p) (d-trim-string class))))))

;;(cname foo)
(defun cfm--get-lisp++-method ()
  (save-excursion
    (let (method p)
      (setq p (point))
      (when (re-search-backward (concat "^ (\\(cmethod\\|"
                                        "c-static-method\\|"
                                        "c-constructor-method\\|"
                                        "c-destructor-method\\|"
                                        "cfriend\\)") nil t)
        (when (re-search-forward "(cname \\(~?[a-zA-Z_][a-zA-Z0-9_]*\\))"
                                 (point-at-eol) t)
          (setq method (buffer-substring-no-properties
                        (match-beginning 1)
                        (match-end 1)))
          (beginning-of-line)
          (forward-sexp 1)
          (if (> (point) p) (d-trim-string method)))))))

(defun cfm--get-lisp++-function ()
  (save-excursion
    (let (name p)
      (setq p (point))
      (when (re-search-backward "^(cfunction" nil t)
        (when (re-search-forward "(cname \\([a-z_][a-zA-Z0-9_]+\\))"
                                 (point-at-eol) t)
          (setq name (buffer-substring-no-properties
                      (match-beginning 1)
                      (match-end 1)))
          (beginning-of-line)
          (forward-sexp 1)
          (if (> (point) p) (d-trim-string name)))))))

(defun cfm--get-jtw-decl ()
  (let (p p1 p2 p3
          decl1 name1
          decl2 name2
          decl3 name3
          decl name
          str1 str2 str3)
    (save-match-data
      (save-excursion
        (setq decl "")
        (setq name "")
        (setq str1 (concat "^[ \t]*\\(public +\\|private +\\|protected +\\|abstract +\\|final +\\)*"
                           "\\(function\\|property\\|method\\|classVar\\)"
                           "[ \t]+[A-Za-z][a-zA-Z0-9_<>]+[][]*"
                           "[ \t]+\\([a-z][a-zA-Z0-9_]*\\)[ \t]*[()=;]"))
        (setq str2 (concat "^[ \t]*\\(public +\\|private +\\|protected +\\|\\)\\(constructor\\)"
                           "[ \t]+\\([A-Z][a-zA-Z0-9_]*\\)("))
        (setq str3 "^[ \t]*\\(beginMain\\>\\)")
        (beginning-of-line)
        (setq p (point))
        ;; ------------------------------------------------------------
        (goto-char p)
        (save-excursion
          (setq p1 (or (if (looking-at str1) (point))
                       (re-search-backward str1 nil t)))
          (when p1
            (setq decl1 (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
            (setq name1 (buffer-substring-no-properties (match-beginning 3) (match-end 3))))
          )
        (save-excursion
          (setq p1-a (re-search-backward "^end[ \t\r\n]" nil t))
          )
        (if (and p1-a p1 (> p1-a p1))
            (setq p1 nil))
        ;; ------------------------------------------------------------
        (goto-char p)
        (setq p2 (or (if (looking-at str2) (point))
                     (re-search-backward str2 nil t)))
        (when p2
          (setq decl2 (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
          (setq name2 (buffer-substring-no-properties (match-beginning 3) (match-end 3))))
        ;; ------------------------------------------------------------
        (goto-char p)
        (setq p3 (or (if (looking-at str3) (point))
                     (re-search-backward str3 nil t)))
        (when p3
          (setq decl3 "")
          (setq name3 (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
        ;; ------------------------------------------------------------
        ;;(if (and (not p1) (not p2) debug-on-error) (debug "both null"))
        ;;(if debug-on-error (debug "Grease is the word"))
        (when (or p1 p2 p3)
          ;;(if debug-on-error (debug "A good heart these days is hard to find"))
          (when (not p1) (setq p1 (point-min)))
          (when (not p2) (setq p2 (point-min)))
          (when (not p3) (setq p3 (point-min)))
          (cond
           ((and (>= p1 p2) (>= p1 p3))
            (setq decl decl1)
            (setq name name1))
           ((and (>= p2 p1) (>= p2 p3))
            (setq decl decl2)
            (setq name name2))
           ((and (>= p3 p1) (>= p3 p2))
            (setq decl decl3)
            (setq name name3))
           (t
            (debug "Should never happen"))
           ))
        (cons (d-trim-string decl) (d-trim-string name))
        ))))

(defun cfm--get-jtw-class-or-interface ()
  (save-excursion
    (let (class-or-interface name str)
      (setq str "\\<\\(class +\\|interface +\\)\\([A-Z][a-zA-Z0-9_]*\\)")
      (when (or (looking-at str)
                (re-search-backward str nil t))
        (setq class-or-interface (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
        (setq name               (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
        (list nil (d-trim-string name) (d-trim-string class-or-interface))))))

;;; (setq i 1)
;;; (setq was-abstract-method nil)
;;; (cfm--get-method 0)
;;; (progn (set-buffer "AnimalTest.java") (cfm--get-method 1))
(defun cfm--get-method (i)
  "Gets current method in current buffer"
  (block nil
    (let ((case-fold-search    nil)
          (p0                  nil)
          (p1                  nil)
          (p2                  nil)
          (p3                  nil)
          (p4                  nil)
          (bra                 nil)
          (end                 nil)
          (str                 nil)
          (args                nil)
          (done                nil)
          (name                nil)
          (orig                nil)
          (found               nil)
          (result              nil)
          (class-name          nil)
          (class-decl          nil)
          (was-abstract-method nil)
          (was-all-on-one-line nil)
          (spaces              (make-string (* i c-basic-offset) ? ))
          )
      (save-match-data
        (save-excursion
          (setq p0 (point)) ;;       1                           2                          3
          (setq search-str "^[ \t]*\\(public +\\|abstract +\\)*\\(class +\\|interface +\\)\\([A-Z][a-zA-Z0-9_]*\\)[ \t\r\n]")
          (cond
           ((save-excursion
              (beginning-of-line)
              (looking-at search-str))
            (setq class-decl (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
            (setq class-name (buffer-substring-no-properties (match-beginning 3) (match-end 3)))
            (return (list (d-trim-string class-decl) (d-trim-string class-name) 'Sinner-man))
            )
           ((re-search-backward search-str nil t)
            ;;(debug "Ludwig Van Beethoven")
            (setq class-decl (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
            (setq class-name (buffer-substring-no-properties (match-beginning 3) (match-end 3)))
            (setq found t)
            )
           )
          (progn
            (setq found nil)
            (forward-line 1)
            (beginning-of-line)
            (skip-chars-forward " \t\r\n")
            (setq p1 (point))
            (when (and 456 (looking-at "{"))
              (condition-case err
                  (forward-sexp 1)
                (error nil))
              (setq p2 (point))
              (goto-char p0)))
          ;;                                        1                                                                          2                                                                                                                    3                             4
          ;;(setq str-void-main (concat "^" spaces "\\(public +\\|protected +\\|private +\\|final +\\|abstract +\\|static +\\)*\\(void +\\|boolean[][]* +\\|char[][]* +\\|int[][]* +\\|float[][]* +\\|double[][]* +\\|[A-Z][a-zA-Z0-9_<>]*[][]* +\\)\\([a-z][a-zA-Z0-9_]*\\)[ \t]*\\(([^()]*)\\)"))
          ;;(setq str (concat "^" spaces "\\(public +\\|protected +\\|private +\\|final +\\|abstract +\\|static +\\)*\\(void +\\|boolean[][]* +\\|short[][]* +\\|int[][]* +\\|long[][]* +\\|float[][]* +\\|double[][]* +\\|[A-Z][a-zA-Z0-9_<>,]*[][]* +\\)\\([a-z][a-zA-Z0-9_]*[ \t]*\\)\\(([^()]*)\\)"))
          ;;                                                                      1                   2
          (setq str (concat "^" spaces "[a-z][-a-zA-Z0-9_ .:!@#$%^&*/+<>]*[ \t]+\\([a-zA-Z0-9_:]+\\)\\(([^()]*)\\)"))
          ;;(if (< p0 p2)
          ;;  (narrow-to-region p1 p2))
          (when (save-excursion
                  (beginning-of-line)
                  (looking-at str))
            (setq found t))
          (when (re-search-backward str nil t)
            (setq found t))
          ;; ---------------------------------------------------------
          (when found
            (setq p (point))
            (goto-char p0)
            (beginning-of-line)
            (setq orig (point))
            (setq found nil)
            (setq name nil)
            (setq args nil)
            (cond
             ;; ------------------------------------------------------
             ((save-excursion
                (beginning-of-line)
                (looking-at str))
              (setq name (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
              (setq args (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
              ;;(if debug-on-error (debug 666))
              (end-of-line)
              (forward-line 1)
              (beginning-of-line)
              (if (not (looking-at "[ \t]*{"))
                  (return (list name (concat args ";") 'whistle)))
              ;;(message "I have a bird to whistle and I have a bird to sing")
              (setq p (point))
              (setq found t))
             ;; ------------------------------------------------------
             ((re-search-backward str nil t)
              ;;(debug "No name")
              (setq name (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
              (setq args (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
              ;;(if debug-on-error (debug "Feng Shui"))
              (search-forward "{" (save-excursion
                                    (forward-line 1)
                                    (point-at-eol)) t)
              (forward-char -1)
              ;;(assert (looking-at "[ \t]*{"))
              ;;(message "Some people tell me that the word blues ain't bad")
              (setq p (point))
              (setq found t))
             ;; ------------------------------------------------------
             )
            ;;(debug "Miles Davis: Birth Of The Cool")
            ;; -------------------------------------------------------
            (when found
              (goto-char p)
              (skip-chars-forward " \t\r\n")
              (when (and 7 (looking-at "{"))
                (setq bra (point))
                ;;(if (and debug-on-error (not (looking-at "{")))
                ;;(if debug-on-error (debug "Rocketman"))
                (when (save-excursion
                        (forward-char 1)
                        (cfm--inside (point) 1))
                  ;;(debug "salami")
                  (beginning-of-line)
                  (forward-line -1)
                  (search-forward "(" bra)
                  ;;(debug "sardines")
                  ;;(debug "hot tomales")
                  (forward-char -1)
                  (setq end (point))
                  (if (search-backward "operator" (point-at-bol) t)
                      (skip-chars-backward "a-z") ;; skip over operator
                    (search-forward "(" (point-at-eol))
                    (save-excursion
                      (forward-char -1)
                      (forward-sexp 1)
                      (setq p2 (point))
                      (if (not args)
                          (setq args (buffer-substring-no-properties end p2))))
                    (beginning-of-line)
                    (setq p2 (point))
                    (skip-chars-forward "a-zA-Z0-9_:"))
                  (if (not name)
                      (setq name (buffer-substring-no-properties p2 (point))))
                  ;;(if debug-on-error (debug "Dazed and Confused name=%s args=%s" name args))
                  )))
            (setq result (list (d-trim-string name)
                               (d-trim-string args)
                               (if class-decl (d-trim-string class-decl) "")
                               (if class-name (d-trim-string class-name) "")))
            )
          )
        )
      result
      ) ;; END LET!
    ) ;; END BLOCK!
  )

(defun cfm--get-cfunction ()
  (let ((str    "^(cfunction (cret [a-zA-Z0-9_]+[&*]*) (cname \\([a-zA-Z0-9_]+\\)")
        (result nil))
    (save-excursion
      (beginning-of-line)
      (if (or (looking-at str)
              (re-search-backward str nil t))
          (setq result (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
      (d-trim-string result))))

;; (setq i 1)
(defun cfm--get-class::method (i)
  (let ((case-fold-search nil)
        (bra              nil)
        (orig             (point))
        (end              nil)
        )
    (save-match-data
      (save-excursion
        (when (and (re-search-backward (concat "^"
                                               (make-string
                                                (* i c-basic-offset) ? )
                                               "{")
                                       nil
                                       t) (cfm--inside orig i))

          (setq bra (point))
          (skip-chars-forward " \t")
          (assert (and 789 (looking-at "{")))
          (forward-line -1)
          ;;(while (looking-at "^[ \t]+") (forward-line -1))
          ;;(if (looking-at "^STAR_OK") (re-search-forward "(" nil t))
          (when (re-search-forward "(" bra t)
            (forward-char -1)
            (setq end (point))
            (skip-chars-backward "_a-zA-Z0-9")
            (if (d-delta-looking-at "~" -1)
                (forward-char -1))
            (if (d-delta-looking-at "::" -2)
                (progn
                  (forward-char -2)
                  (skip-chars-backward "_a-zA-Z0-9")
                  ;;(d-foo)
                  (buffer-substring-no-properties (point) end))
              ;;(d-foo)
              (d-trim-string (buffer-substring-no-properties (point) end)))))))))

(defun cfm--get-php-function ()
  (save-excursion
    (save-match-data
      (let (name)
        (when (or (save-excursion
                    (beginning-of-line)
                    (looking-at "^[ \t]*function \\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]*\\(([^()]*)\\)"))
                  (re-search-backward "^[ \t]*function \\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]*\\(([^()]*)\\)" nil t))
          (setq name (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
          (setq args (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
          (cons (d-trim-string name) (d-trim-string args)))))))

(defun cfm--get-compilation-strobe ()
  (save-excursion
    (save-match-data
      (when (save-excursion
              (forward-line 1)
              (re-search-backward "\\*\\*\\*\\* STROBE=\"[a-zA-Z0-9]*\"" nil t))
        (d-trim-string (buffer-substring-no-properties (match-beginning 0) (match-end 0)))))))

;;;
;;; last
;;;
;;(setcar (last mode-line-format) 'cfm--class::method)
;;(setcdr (last mode-line-format) (cons "-%-" nil))

;;;
;;; first
;;;
;;(setcar mode-line-format 'cfm--class::method)
;;(setq-default mode-line-format (cons "-" mode-line-format))

;;(setq minor-mode-alist (cons '(t cfm--class::method) minor-mode-alist))
;;(last minor-mode-alist)

;;(setcdr (last minor-mode-alist) (cons '(t cfm--class::method) nil))

(setq minor-mode-alist (cons '(t cfm--class::method) minor-mode-alist))

;;(setq cfm--class::method " Foo::smeg")
;;(setq cfm--class::method " Peek::poke")

(provide 'cfm)
;;; cfm.el ends here

