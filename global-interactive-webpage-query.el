;;; global-interactive-webpage-query.el --- Send Request to get a web page, and interactive query page info.  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'yaml)
(require 'subr-x)
(require 'global-interactive-emacs)

(add-to-list 'global-interactive-default-command
             (list "Webpage Query" #'global-interactive-webpage-query))

(defvar global-interactive-python-shell-path nil)
(defvar global-interactive-webpage-yaml-path nil)
(defvar global-interactive-webpage-plist nil)

(defun global-interactive-webpage-init ()
  "Load pre-defined webpage yaml file and parse it to 'global-interactive-webpage-plist'."
  (when (and
         (not global-interactive-webpage-plist)
         global-interactive-webpage-yaml-path)
    (setq global-interactive-webpage-plist
          (yaml-parse-string
           (global-interactive-file-to-string global-interactive-webpage-yaml-path)
           :object-type 'plist
           :sequence-type 'array
           :null-object :empty))))

(defun global-interactive--keys-to-names (keys)
  "Convert yaml keys to names.
KEYS is yaml symbol like : ':movie'."
  (mapcar
   (lambda (key) (string-remove-prefix ":" key))
   (mapcar #'symbol-name keys)))

(defun global-interactive--find-key (name keys)
  "Find key in KEYS by NAME."
  (seq-find
   (lambda (key)
     (string= name (string-remove-prefix ":" (symbol-name key))))
   keys))

(defun global-interactive--find-value-in-vector-by-key (key the-vector)
  "Find value in THE-VECTOR by KEY."
  (plist-get
   (car
    (seq-filter
     (lambda (item) (eq (car item) key))
     the-vector))
   key))


(defun global-interactive-webpage--send-request (url method body user password)
  "Send http request.
URL: url, required
METHOD: method, required
BODY: body, optional
USER: auth user, optional
PASSWORD: auth password, optional"
  (let* ((command
          (format "python3 %s '%s' '%s'" global-interactive-python-shell-path method url))
         (command (if body (format "%s -b '%s' " command body) command))
         (command (if user (format "%s -u '%s' " command user) command))
         (command
          (if password (format "%s -p '%s' " command password) command)))
    (global-interactive-log command)
    (shell-command-to-string command)))
    

(defun global-interactive-webpage-query ()
  "Interactively select webpage and query it."
  (interactive)
  (global-interactive-webpage-init)
  (let* ((keys
          (remove-if-not #'symbolp global-interactive-webpage-plist))
         (selected-item
          (completing-read "Choose Webpage: "
                           (global-interactive--keys-to-names keys)))
         (key (global-interactive--find-key selected-item keys))
         (value (plist-get global-interactive-webpage-plist key))
         (url (plist-get value :url))
         (user (plist-get value :user))
         (password (plist-get value :password))
         (method (plist-get value :method))
         (body (plist-get value :body))
         (candidates-fun (plist-get value :candidates-fun))
         (url-fun (plist-get value :url-fun))
         (response (global-interactive-webpage--send-request url method body user password))
         (temp (global-interactive-log response))
         (candidates (funcall (intern candidates-fun) response))
         (urls (funcall (intern url-fun) response url))
         (selected-item (completing-read "Query in webpage: " candidates))
         (index (cl-position selected-item candidates :test #'equal))
         (url (nth index urls)))
    (shell-command-to-string (format "open '%s'" url))
    ))

(defun global-interactive-webpage-update ()
  "Update 'global-interactive-webpage-plist'."
  (interactive)
  (when global-interactive-webpage-yaml-path
    (setq global-interactive-webpage-plist
          (yaml-parse-string
           (global-interactive-file-to-string global-interactive-webpage-yaml-path)
           :object-type 'plist
           :sequence-type 'array
           :null-object :empty))))



(provide 'global-interactive-webpage-query)
;;; global-interactive-webpage-query.el ends here
