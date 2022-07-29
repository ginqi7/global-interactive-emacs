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
(add-to-list 'global-interactive-default-command
             (list "Run App" #'global-interactive-run-app))

(defvar global-interactive-app-command nil)

(defun global-interactive-app-command--find-all-app ()
  "Find all apps in system."
  (let* ((output
          (split-string
           (shell-command-to-string "ls -1 /Applications")
           "\n" t " +")))
    (setq global-interactive-app-command
          (mapcar
           (lambda (path) (string-remove-suffix ".app" path))
           output))))


(defun global-interactive-run-app ()
  "Interactively select app and open it."
  (interactive)
  (if global-interactive-app-command
      (let* ((candidates global-interactive-app-command)
             (selected-item (completing-read "" candidates))
             (command
              (seq-filter
               (lambda (command) (string= selected-item command))
               global-interactive-app-command)))
        (when command
          (shell-command (format "open -a \"%s\"" (car command)))))
    (progn
      (completing-read "Warnning"
                       '("candidates is null,Maybe there is looking for the app in the computer"))
      (global-interactive-run-app-init))))


(defun global-interactive-run-app-init()
  "Init list all apps in system."
  (when (not global-interactive-app-command)
    (global-interactive-app-command--find-all-app)))


(provide 'global-interactive-run-app)
;;; global-interactive-run-app.el ends here
