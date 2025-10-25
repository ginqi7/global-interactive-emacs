;;; global-interactive-find-file.el --- Global interactively find file  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Qiqi Jin

;; Author: Qiqi Jin(require 'global-interactive-emacs) <ginqi7@gmail.com>
;; Keywords: lisp, tools

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
;;  `global-interactive-find-file'
;;    Global interactively find file and do an action on this file.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:
(require 'cl-seq)
(require 'global-interactive-emacs)
(add-to-list 'global-interactive-default-command
             (list "Find File" #'global-interactive-find-file))

(defvar global-interactive-find-file-base-directory "~/")

(defvar global-interactive-find-file-actions
  '("Copy File Simple Name"
    "Copy File Full Name"
    "Copy File Path"
    "Copy File Content"
    "Open file"))


(defun global-interactive-parse-ls-single (file-item)
  "Parse file item to select candidate string.
FILE-ITEM is a list contains file metadata"
  (let* ((item-split (split-string file-item " +" t))
         (file-type (car item-split))
         (file-name (car (last item-split))))
    (when (eq (length item-split) 9)
      (list file-name
            (if (string-prefix-p "d" file-type)
                "(Directory)"
              "(File)")))))

(defun global-interactive-parse-ls-output (output-list base-path)
  "Recursively parse `ls` output and select one of output.
OUTPUT-LIST is last ls' output.
BASE-PATH is last ls' path."
  (let* ((candidates
          (remove nil
                  (mapcar #'global-interactive-parse-ls-single output-list)))
         (candidates
          (mapcar
           (lambda (item)
             (let* ((file-name (car item))
                    (file-type (car (cdr item)))
                    (file-type
                     (if (string= file-name ".")
                         (format "(Current Directory is [%s])" base-path)
                       file-type)))
               (format "%s  %s " file-name file-type)))
           candidates))
         (seletcted-item (global-interactive-read "Please Select a file: " candidates))
         (file-name (car (split-string seletcted-item "(" t " +")))
         (file-path (format "%s/%s" base-path file-name)))
    (if (cl-search "(Directory)" seletcted-item)
        (global-interactive-find-file file-path)
      (global-interactive-do-action-on-file file-path))))

(defun global-interactive-do-action-on-file (file-path)
  "Do some action in file named 'FILE-PATH'."
  (let ((selected-item
         (completing-read "Please Select an action for file: "
                          global-interactive-find-file-actions)))

    (pcase (cl-position selected-item global-interactive-find-file-actions :test #'string=)

      (0 (kill-new (file-name-directory file-path)))
      (1
       (kill-new (car (last (split-string file-path "/" t " +")))))
      (2 (kill-new file-path))
      (3 (kill-new (global-interactive-file-to-string file-path)))
      (4 (shell-command-to-string (format "open %s" file-path))))))

(defun global-interactive-find-file (&optional file-path)
  "Global interactively find file and do an action on this file.
FILE-PATH is select file path."
  (interactive)
  (let* ((file-path
          (if file-path
              file-path
            global-interactive-find-file-base-directory))
         (ls-output
          (shell-command-to-string (format "ls -la %s" file-path)))
         (output-list (split-string ls-output "\n" t " +")))
    (global-interactive-parse-ls-output output-list file-path)))

(provide 'global-interactive-find-file)
;;; global-interactive-find-file.el ends here
