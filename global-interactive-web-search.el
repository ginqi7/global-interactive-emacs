;;; global-interactive-web-search.el --- Golobal interactively search in pre-defined engine  -*- lexical-binding: t; -*-

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
;;  `global-interactive-web-search'
;;    Interactively query by web engine.
;;  `global-interactive-web-search-update'
;;    Update 'global-interactive-web-search-plist'.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:

(require 'yaml)
(require 'global-interactive-emacs)
(add-to-list 'global-interactive-default-command
             (list "Web Search" #'global-interactive-web-search))

(defvar global-interactive-web-search-plist nil)

(defvar global-interactive-web-search-yaml-path nil)

(defun global-interactive-web-search-init ()
  "Init parse pre-defined yaml file to 'global-interactive-web-search-plist'."
  (when (and
         (not global-interactive-web-search-plist)
         global-interactive-web-search-yaml-path)
    (setq global-interactive-web-search-plist
          (yaml-parse-string
           (global-interactive-file-to-string global-interactive-web-search-yaml-path)
           :object-type 'plist
           :sequence-type 'array
           :null-object :empty))))

(defun global-interactive--keys-to-names (keys)
  "Convert yaml keys to names.
KEYS is yaml symbol like : ':google'."
  (mapcar
   (lambda (key) (string-remove-prefix ":" key))
   (mapcar #'symbol-name keys)))

(defun global-interactive--find-key (name keys)
  "Find key in KEYS by NAME."
  (car
   (seq-filter
    (lambda (key)
      (string= name (string-remove-prefix ":" (symbol-name key))))
    keys)))

(defun global-interactive--find-value-in-vector-by-key (key the-vector)
  "Find value in THE-VECTOR by KEY."
  (plist-get
   (car
    (seq-filter
     (lambda (item) (eq (car item) key))
     the-vector))
   key))

(defun global-interactive-web-search (&optional url-expr)
  "Interactively query by web engine.
URL-EXPR is yaml object."
  (interactive)
  (global-interactive-web-search-init)
  (cond
   ((stringp url-expr)
    (browse-url
     (string-replace "${param}"
                     (completing-read "Input your search keyword: "
                                      '("Input your search keyword: "))
                     url-expr)))
   ((vectorp url-expr)
    (let* ((keys (mapcar #'car url-expr))
           (selected-item
            (completing-read "Choose Url label: "
                             (global-interactive--keys-to-names keys)))
           (key (global-interactive--find-key selected-item keys))
           (url-expr
            (global-interactive--find-value-in-vector-by-key key url-expr)))
      (when url-expr (global-interactive-web-search url-expr))))
   ((not url-expr)
    (let* ((url-plist global-interactive-web-search-plist)
           (keys
            (seq-filter #'symbolp global-interactive-web-search-plist))
           (selected-item
            (completing-read "Choose Url label: "
                             (global-interactive--keys-to-names keys)))
           (key (global-interactive--find-key selected-item keys))
           (url-expr (plist-get url-plist key)))
      (when url-expr (global-interactive-web-search url-expr))))))

(defun global-interactive-web-search-update ()
  "Update 'global-interactive-web-search-plist'."
  (interactive)
  (when global-interactive-web-search-yaml-path
    (setq global-interactive-web-search-plist
          (yaml-parse-string
           (global-interactive-file-to-string global-interactive-web-search-yaml-path)
           :object-type 'plist
           :sequence-type 'array
           :null-object :empty))))

(provide 'global-interactive-web-search)
;;; global-interactive-web-search.el ends here

