;;; global-interactive-run-app.el --- Global interactively run app in system  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords: tools, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `global-interactive-run-app'
;;    Interactively select app and open it.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:


(require 'global-interactive-emacs)

(defun global-interactive-run-app--find-all-app ()
  "Find all apps in system."
  (split-string
   (shell-command-to-string "ls -1 /Applications")
   "\n" t " +"))


(defun global-interactive-run-app (app)
  "Open a APP."
  (print app)
  (shell-command (format "open -a \"%s\"" app)))

(defun global-interactive-app-hashtable ()
  "Build hash table."
  (let ((hashmap (make-hash-table :test 'equal)))
    (dolist (app (global-interactive-run-app--find-all-app))
      (let ((key (intern app))
            (value app))
        (puthash key value hashmap)))
    hashmap))

(defun global-interactive-run-app--update-candidates ()
  "Update url candidates."
  (puthash 'app (global-interactive-app-hashtable) global-interactive-emacs--actions-table)
  (puthash 'app 'global-interactive-run-app global-interactive-emacs--actions-func))

(advice-add 'global-interactive-emacs--update-candidates :before #'global-interactive-run-app--update-candidates)

(provide 'global-interactive-run-app)
;;; global-interactive-run-app.el ends here
