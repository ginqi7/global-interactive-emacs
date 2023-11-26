;;; global-interactive-kill-app.el --- Kill App for Global Interactive Emacs.  -*- lexical-binding: t; -*-

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
;; (require 'global-interactive-kill-app)
;; ```
;;

;;; Code:


(require 'global-interactive-emacs)

(defun global-interactive-kill-app--running-apps ()
  "List all running apps in system."
  (macc-list-all-running-apps))


(defun global-interactive-running-apps-hashtable ()
  "Build hash table."
  (let ((hashmap (make-hash-table :test 'equal)))
    (dolist (app (global-interactive-kill-app--running-apps))
      (let ((key (intern app))
            (value app))
        (puthash key value hashmap)))
    hashmap))

(defun global-interactive-kill-app--update-candidates ()
  "Update url candidates."
  (puthash 'kill-app (global-interactive-running-apps-hashtable) global-interactive-emacs--actions-table)
  (puthash 'kill-app 'macc-kill-app global-interactive-emacs--actions-func))

(advice-add 'global-interactive-emacs--update-candidates :before #'global-interactive-kill-app--update-candidates)

(provide 'global-interactive-kill-app)
;;; global-interactive-kill-app.el ends here
