;;; global-interactive-chrome-bookmarks.el --- Global Interactive Query Chrome bookmarks  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Qiqi Jin

;; Author: Qiqi Jin(require 'global-interactive-emacs) <ginqi7@gmail.com>
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
;;  `global-interactive-chrome-bookmarks'
;;    Global interactive Query Chrome Bookmarks.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:

(require 'subr-x)
(require 'global-interactive-emacs)
(add-to-list 'global-interactive-default-command
             (list "Chrome Bookmarks" #'global-interactive-chrome-bookmarks))

(defvar global-interactive-chrome-bookmarks-path
  (cond
   ((eq system-type 'darwin)
    "~/Library/Application Support/Google/Chrome/Default/Bookmarks")
   ((eq system-type 'gnu/linux)
    "~/.config/google-chrome/Default/Bookmarks")))

(defvar global-interactive-chrome-bookmarks-list nil)

(defun global-interactive-chrome-bookmark-keys-p (keys)
  "Check if a json hash table is a bookmark item.
KEYS hashtable keys."
  (and
   (member "date_added" keys)
   (member "guid" keys)
   (member "id" keys)
   (member "name" keys)
   (member "type" keys)
   (member "url" keys)))

(defun global-interactive-chrome-parse-json-to-list (json)
  "Parse Chrome Bookmarks JSON hash table to a elisp list.
JSON is chrome bookmarks json hash table."
  (cond
   ((not json)
    nil)
   ((and
     (hash-table-p json)
     (global-interactive-chrome-bookmark-keys-p
      (hash-table-keys json)))
    (push json global-interactive-chrome-bookmarks-list))
   ((hash-table-p json)
    (mapcar #'global-interactive-chrome-parse-json-to-list
            (hash-table-values json)))
   ((vectorp json)
    (mapcar #'global-interactive-chrome-parse-json-to-list json))))

(defun global-interactive-chrome-bookmarks-init ()
  "Read Chrome bookmarks file.
Parse it to 'global-interactive-chrome-bookmarks-list'."
  (when (not global-interactive-chrome-bookmarks-list)
    (global-interactive-chrome-parse-json-to-list
     (json-parse-string
      (global-interactive-file-to-string global-interactive-chrome-bookmarks-path)))))

(defun global-interactive-chrome-bookmarks ()
  "Global interactive Query Chrome Bookmarks."
  (interactive)
  (global-interactive-chrome-bookmarks-init)
  (let* ((candidates
          (mapcar
           (lambda (item) (gethash "name" item))
           global-interactive-chrome-bookmarks-list))
         (selected-item (completing-read "Select Bookmark" candidates))
         (url
          (gethash "url"
                   (seq-find
                    (lambda (item)
                      (string= selected-item (gethash "name" item)))
                    global-interactive-chrome-bookmarks-list))))
    (shell-command (format "open \"%s\"" url))))

(provide 'global-interactive-chrome-bookmarks)
;;; global-interactive-chrome-bookmarks.el ends here
