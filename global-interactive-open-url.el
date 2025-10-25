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


(defvar global-interactive-url-plist nil)

(defvar global-interactive-url-hashtable nil)


(defun global-interactive-open-url-init ()
  "Load pre-defined url yaml file and parse it to 'global-interactive-url-plist'."
  (when (and
         (not global-interactive-url-hashtable)
         global-interactive-url-yaml-path)
    (setq global-interactive-url-hashtable
          (yaml-parse-string
           (global-interactive-file-to-string global-interactive-url-yaml-path)
           :sequence-type 'list
           :null-object :empty))))


(defun global-interactive-open-url-update ()
  "Update 'global-interactive-url-plist'."
  (interactive)
  (when global-interactive-url-yaml-path
    (setq global-interactive-url-hashtable
          (yaml-parse-string
           (global-interactive-file-to-string global-interactive-url-yaml-path)
           :sequence-type 'list
           :null-object :empty))))


(defun global-interactive-open-url--update-candidates ()
  "Update url candidates."
  (puthash 'url global-interactive-url-hashtable global-interactive-emacs--actions-table)
  (puthash 'url 'browse-url global-interactive-emacs--actions-func))


(advice-add 'global-interactive-emacs--update-candidates :before #'global-interactive-open-url--update-candidates)

(provide 'global-interactive-open-url)
;;; global-interactive-open-url.el ends here
