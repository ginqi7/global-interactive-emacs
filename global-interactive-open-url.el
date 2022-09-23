;;; global-interactive-open-url.el --- Global interactive open url   -*- lexical-binding: t; -*-

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
(require 'global-interactive-emacs)
(add-to-list 'global-interactive-default-command
             (list "Open Url" #'global-interactive-open-url))

(defvar global-interactive-url-plist nil)

(defvar global-interactive-url-yaml-path nil)

(defun global-interactive-open-url-init ()
  "Load pre-defined url yaml file and parse it to 'global-interactive-url-plist'."
  (when (and
         (not global-interactive-url-plist)
         global-interactive-url-yaml-path)
    (setq global-interactive-url-plist
          (yaml-parse-string
           (global-interactive-file-to-string global-interactive-url-yaml-path)
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

(defun global-interactive-open-url (&optional url-expr)
  "Interactively select url and open it.
URL-EXPR is a yaml object."
  (interactive)
  (global-interactive-open-url-init)
  (cond
   ((stringp url-expr)
    (browser-url url-expr))
   ((vectorp url-expr)
    (let* ((keys (mapcar #'car url-expr))
           (selected-item
            (completing-read "Choose Url label: "
                             (global-interactive--keys-to-names keys)))
           (key (global-interactive--find-key selected-item keys))
           (url-expr
            (global-interactive--find-value-in-vector-by-key key url-expr)))
      (when url-expr (global-interactive-open-url url-expr))))
   ((not url-expr)
    (let* ((url-plist global-interactive-url-plist)
           (keys (seq-filter #'symbolp global-interactive-url-plist))
           (selected-item
            (completing-read "Choose Url label: "
                             (global-interactive--keys-to-names keys)))
           (key (global-interactive--find-key selected-item keys))
           (url-expr (plist-get url-plist key)))
      (when url-expr (global-interactive-open-url url-expr))))))

(defun global-interactive-open-url-update ()
  "Update 'global-interactive-url-plist'."
  (interactive)
  (when global-interactive-url-yaml-path
    (setq global-interactive-url-plist
          (yaml-parse-string
           (global-interactive-file-to-string global-interactive-url-yaml-path)
           :object-type 'plist
           :sequence-type 'array
           :null-object :empty))))

 (provide 'global-interactive-open-url)
;;; global-interactive-open-url.el ends here
