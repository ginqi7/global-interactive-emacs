;;; global-interactive-chrome-tab.el --- Global switch Google Chrome tab with emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
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

;;; Code:

(require 'global-interactive-emacs)

(defcustom global-interactive-browser-app-name
  "Brave Browser"
  "")

(defvar global-interactive-browser-tab-list-jsa-path
  (expand-file-name "list-browser-tab.js"
                    (file-name-directory
                     (or load-file-name
                         (buffer-file-name)))))

(defvar global-interactive-browser-tab-active-jsa-path
  (expand-file-name "active-browser-tab.js"
                    (file-name-directory (or load-file-name
                                             (buffer-file-name)))))

(defvar global-interactive-browser-tab-close-jsa-path
  (expand-file-name "close-browser-tab.js"
                    (file-name-directory (or load-file-name
                                             (buffer-file-name)))))


(defun global-interactive-browser-tab-all ()
  (let ((cmd (format "%s  -l JavaScript %s \"%s\""
                     (executable-find "osascript")
                     global-interactive-browser-tab-list-jsa-path
                     global-interactive-browser-app-name)))
    (json-parse-string (shell-command-to-string cmd)
                       :array-type 'list
                       :null-object "")))

(defun global-interactive-browser-tab-actions (tab)
  "Open a APP."
  (let ((table (make-hash-table :test #'equal)))
    (puthash "Active"
             (global-interactive-emacs-candidate
              :key "Active"
              :value tab
              :next-func #'global-interactive-browser-tab-active)              
             table)
    (puthash "Close"
             (global-interactive-emacs-candidate
              :key "Close"
              :value tab
              :next-func #'global-interactive-browser-tab-close)              
             table)
    table))

(defun global-interactive-browser-tab-close (tab)
  (let ((cmd (format "%s  -l JavaScript %s \"%s\" %s"
                     (executable-find "osascript")
                     global-interactive-browser-tab-close-jsa-path
                     global-interactive-browser-app-name
                     (gethash "id" tab))))
    (shell-command-to-string cmd)
    (global-interactive-emacs-quit-back)))

(defun global-interactive-browser-tab-active (tab)
  (let ((cmd (format "%s  -l JavaScript %s \"%s\" %s"
                     (executable-find "osascript")
                     global-interactive-browser-tab-active-jsa-path
                     global-interactive-browser-app-name
                     (gethash "id" tab))))
    (shell-command-to-string cmd)
    (global-interactive-emacs-quit-back)))

(defun global-interactive-browser-tab-table (value)
  (let ((table (make-hash-table :test #'equal)))
    (mapc
     (lambda (tab)
       (puthash (if (string-blank-p (gethash "title" tab))
                    (gethash "url" tab)
                  (gethash "title" tab))
                (global-interactive-emacs-candidate
                 :key (if (string-blank-p (gethash "title" tab))
                          (gethash "url" tab)
                        (gethash "title" tab))
                 :value tab
                 :next-table nil
                 :next-func #'global-interactive-browser-tab-actions
                 :preview (lambda (tab) (format "Title: %s\nURL:%s\n"
                                                (gethash "title" tab)
                                                (gethash "url" tab))))
                table))
     (global-interactive-browser-tab-all))
    table))

(puthash
 "Browser Tab"
 (global-interactive-emacs-candidate
  :key "Browser Tab"
  :value "Browser Tab"
  :next-table nil
  :preview (lambda (_) 
             (format "Handle your Browser tabs , there are %s tabs in %s"
                     (length (global-interactive-browser-tab-all))
                     global-interactive-browser-app-name))
  :next-func #'global-interactive-browser-tab-table)
 global-interactive-emacs--candidates-table)


(provide 'global-interactive-browser-tab)
;;; global-interactive-browser-tab.el ends here
