;;; global-interactive-snippe.el --- snippe for global interactive emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
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

;;; Installation:
;; Manual:
;; Download the source code and put it wherever you like, e.g. into
;; ~/.emacs.d/global-interactive-emacs/
;; ```
;; git clone git@github.com:ginqi7/global-interactive-emacs.git
;; ```
;; Add the downloaded directory to the load path:
;; ```
;; (add-to-list 'load-path "~/.emacs.d/global-interactive-emacs/")
;; (require 'global-interactive-snippe)
;; ```
;;

;;; Code:

(require 'yaml)
(require 'global-interactive-emacs)

(defvar global-interactive-snippe-plist nil)

(defvar global-interactive-snippe-hashtable nil)


(defun global-interactive-open-snippe-init ()
  "Load pre-defined snippe yaml file and parse it to 'global-interactive-snippe-plist'."
  (when (and
         (not global-interactive-snippe-hashtable)
         global-interactive-snippe-yaml-path)
    (setq global-interactive-snippe-hashtable
          (yaml-parse-string
           (global-interactive-file-to-string global-interactive-snippe-yaml-path)
           :sequence-type 'list
           :null-object :empty))))


(defun global-interactive-open-snippe-update ()
  "Update 'global-interactive-snippe-plist'."
  (interactive)
  (when global-interactive-snippe-yaml-path
    (setq global-interactive-snippe-hashtable
          (yaml-parse-string
           (global-interactive-file-to-string global-interactive-snippe-yaml-path)
           :sequence-type 'list
           :null-object :empty))))


(defun global-interactive-open-snippe--update-candidates ()
  "Update snippe candidates."
  (puthash 'snippe global-interactive-snippe-hashtable global-interactive-emacs--actions-table)
  (puthash 'snippe 'kill-new global-interactive-emacs--actions-func))


(advice-add 'global-interactive-emacs--update-candidates :before #'global-interactive-open-snippe--update-candidates)

(provide 'global-interactive-snippe)
;;; global-interactive-snippe.el ends here
