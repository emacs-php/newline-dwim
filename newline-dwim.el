;;; newline-dwim.el --- Insert a newline and DWIM characters -*- lexical-binding: t -*-

;; Copyright (C) 2017 Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 26 Sep 2017
;; Version: 0.0.1
;; Keywords: convenience
;; URL: https://github.com/emacs-php/newline-dwim
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is in develoment.
;;
;; ## Enable
;;
;;     (bind-key "RET" #'newline-dwim)
;;
;; ## Disable
;;
;;     (bind-key "RET" #'newline)
;;

;;; Code:
(require 'cl-lib)
(require 'newcomment)

;; Customize
(defgroup newline-dwim nil
  "Insert a newline and DWIM charactors."
  :group 'convenience)

(defvar newline-dwim-lighter "dNL ")

;;
(defvar-local newline-dwim-php-comments-alist
  '(("\\(\\s-*\\)//\\(\\s-*\\)" . ("//" . ""))
    ("\\(\\s-*\\)\\*\\(\\s-*\\)" . ("*" . ""))))

;; Variables for let
(defvar newline-dwim--before-pos nil)
(defvar newline-dwim--before-ppss nil)
(defvar newline-dwim--current-pos nil)
(defvar newline-dwim--current-ppss nil)
(defvar newline-dwim--in-process nil)
(defvar newline-dwim--last-line-string nil)


;; Buffer local variables
(defvar-local newline-dwim--setup nil)
(defvar-local newline-dwim-functions nil)
(defvar-local newline-dwim-newline-function #'newline)
(defvar-local newline-dwim-comments-alist nil)


;; Utility
(defun newline-dwim--setup (force)
  "Setup for `newline-dwim' command.  Do refresh when `FORCE' is non-NIL."
  (interactive "p")
  (when (or force (null newline-dwim--setup))
    (setq newline-dwim-functions
          (list (cons #'newline-dwim-continue-comment-test #'newline-dwim-continue-comment)
            ))
    (setq newline-dwim-comments-alist newline-dwim-php-comments-alist)

    (setq newline-dwim--setup t)))

(defun newline-dwim-before-pos ()
  ""
  (let ((before-pos (or newline-dwim--before-pos
                        (save-excursion (end-of-line 0) (point)))))
    (when (and newline-dwim--in-process (null newline-dwim--before-pos))
      (setq newline-dwim--before-pos before-pos))
    before-pos))

(defun newline-dwim-before-ppss ()
  ""
  (let ((before-ppss (or newline-dwim--before-ppss
                        (save-excursion (end-of-line 0) (syntax-ppss)))))
    (when (and newline-dwim--in-process (null newline-dwim--before-ppss))
      (setq newline-dwim--before-ppss before-ppss))
    before-ppss))

(defun newline-dwim-current-pos ()
  ""
  (let ((current-pos (or newline-dwim--current-pos (point))))
    (when (and newline-dwim--in-process (null newline-dwim--current-pos))
      (setq newline-dwim--current-pos current-pos))
    current-pos))

(defun newline-dwim-current-ppss ()
  ""
  (let ((current-ppss (or newline-dwim--current-ppss (syntax-ppss))))
    (when (and newline-dwim--in-process (null newline-dwim--current-ppss))
      (setq newline-dwim--current-ppss current-ppss))
    current-ppss))

(defun newline-dwim-last-line-string ()
  "Return string of last line."
  (let ((last-line-string
         (or newline-dwim--last-line-string
             (save-excursion
              (goto-char (newline-dwim-before-pos))
               (buffer-substring-no-properties (progn (beginning-of-line) (point))
                                               (progn (end-of-line) (point)))))))
    (when (and newline-dwim--in-process (null newline-dwim--last-line-string))
      (setq newline-dwim--last-line-string last-line-string))
    last-line-string))

(defun newline-dwim-string-state (&optional ppss)
  "Return string parse state in `PPSS', `newline-dwim--current-ppss' or current position ppss."
  (nth 3 (or ppss newline-dwim-current-ppss (syntax-ppss))))

(defun newline-dwim-comment-state (&optional ppss)
  "Return comment parse state in `PPSS', `newline-dwim--current-ppss' or current position ppss."
  (nth 3 (or ppss newline-dwim-current-ppss (syntax-ppss))))

(defun newline-dwim-continue-comment-test ()
  ""
  (assoc-default (newline-dwim-last-line-string) newline-dwim-comments-alist 'string-match))

(defun newline-dwim-continue-comment (test-result)
  ""
  (insert (match-string 1 (newline-dwim-last-line-string)) "" (car test-result) (match-string 1 (newline-dwim-last-line-string)) (cdr test-result)))

(defun newline-dwim--1 (func-or-list test-result)
  "Run function or insert charactors by `FUNC-OR-LIST' and pass `TEST-RESULT'."
  (if (functionp func-or-list)
      (funcall func-or-list test-result)
    ""))


;; Command

;;;###autoload
(defun newline-dwim ()
  "Insert a newline and DWIM characters."
  (interactive)
  (newline-dwim--setup nil)
  (save-match-data
    (let ((newline-dwim--in-process t)
          (newline-dwim--before-pos  (point))
          (newline-dwim--before-ppss (syntax-ppss))
          newline-dwim--current-pos newline-dwim--current-ppss newline-dwim--last-line-string)

      (when nil ; suppress warning for unused lexial variables.
        (list newline-dwim--in-process newline-dwim--last-line-string
              newline-dwim--before-pos newline-dwim--before-ppss newline-dwim--current-pos newline-dwim--current-ppss))

      (funcall newline-dwim-newline-function)
      ;; (call-interactively newline-dwim-newline-function)

      (cl-loop for (test . func-or-list) in newline-dwim-functions
               for result = (funcall test)
               if result
               return (newline-dwim--1 func-or-list result)))))

(provide 'newline-dwim)
;;; newline-dwim.el ends here
