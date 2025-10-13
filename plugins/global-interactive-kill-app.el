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

(defvar global-interactive-kill-app--script-path
  (file-name-concat (file-name-directory (or load-file-name (buffer-file-name)))
                    "list_running_app.sh"))

(defun global-interactive-kill-app--running-apps ()
  "List all running apps in system."
  (split-string (shell-command-to-string
                 (format "%s %s"
                         (executable-find "sh")
                         global-interactive-kill-app--script-path))
                "\n" t))

(defun global-interactive-kill-app (app)
  "Open a APP."
  (do-applescript (format "tell application \"%s\" to quit" app))
  (global-interactive-emacs-quit-back))

(defun global-interactive-app-table (value)
  (let ((table (make-hash-table :test #'equal)))
    (mapc
     (lambda (app)
       (puthash app
                (global-interactive-emacs-candidate
                 :key app
                 :value app
                 :next-table nil
                 :next-func #'global-interactive-kill-app)
                table))
     (global-interactive-kill-app--running-apps))
    table))

(puthash
 "Kill App"
 (global-interactive-emacs-candidate
  :key "Kill App"
  :value "Kill App"
  :next-table nil
  :preview (lambda (_) "" (format "Kill a running APP, there are %s running apps"
                                  (length (global-interactive-kill-app--running-apps))))
  :next-func #'global-interactive-app-table)
 global-interactive-emacs--candidates-table)

(provide 'global-interactive-kill-app)
;;; global-interactive-kill-app.el ends here
