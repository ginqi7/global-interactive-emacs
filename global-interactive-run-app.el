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

(defcustom global-interactive-app-directories
  '("/Applications/"
    "/Applications/Utilities"
    "/System/Applications/"
    "/System/Applications/Utilities/"
    "~/Applications/Home\\ Manager\\ Apps/")
  "The directories contains applications.")

(defun global-interactive-run-app--find-all-apps ()
  "Find all apps in system."
  (let ((cmd
         (format "ls %s |  grep '\\.app$' | sed 's/\.app$//g'"
                 (string-join global-interactive-app-directories " "))))
    (split-string
     (shell-command-to-string cmd)
     "\n" t " +")))

(defun global-interactive-run-app (app)
  "Open a APP."
  (shell-command (format "open -a \"%s\"" app)))

(defun global-interactive-app-table (value)
  (print value)
  (let ((table (make-hash-table :test #'equal)))
    (mapc
     (lambda (app)
       (puthash app
                (global-interactive-emacs-candidate
                 :key app
                 :value app
                 :next-table nil
                 :next-func #'global-interactive-run-app)
                table))
     (global-interactive-run-app--find-all-apps))
    (print table)
    table))

(puthash
 "Run App"
 (global-interactive-emacs-candidate
  :key "Run App"
  :value "Run App"
  :next-table nil
  :preview (lambda (_) "" (format "Open a installed APP, there are %s apps"
                                  (length (global-interactive-run-app--find-all-apps))))
  :next-func #'global-interactive-app-table)
 global-interactive-emacs--candidates-table)

(provide 'global-interactive-run-app)
;;; global-interactive-run-app.el ends here
