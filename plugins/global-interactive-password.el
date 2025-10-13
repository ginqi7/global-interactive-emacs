;;; global-interactive-password.el ---               -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords: 

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

(defun global-interactive-password--list ()
  "Find all apps in system."
  (let ((cmd "rbw list --raw"))
    (json-parse-string (shell-command-to-string cmd)
                       :array-type 'list
                       :null-object "")))

(defun global-interactive-password-copy (item)
  "Open a APP."
  (kill-new (shell-command-to-string (string-trim (format "rbw get %s" item))))
  (global-interactive-emacs-quit-back))

(defun global-interactive-password-table (_)
  "Build hash table."
  (let ((hashmap (make-hash-table :test 'equal)))
    (dolist (item (global-interactive-password--list))
      (let ((key (concat (gethash "name" item) "#" (gethash "user" item)))
            (value (gethash "id" item)))
        (puthash key
                 (global-interactive-emacs-candidate
                  :key key
                  :value value
                  :next-table nil
                  :preview (lambda (id)
                             (string-join (cdr (string-split (shell-command-to-string (format "rbw get --full %s" id)))) "\n"))
                  :next-func #'global-interactive-password-copy)
                 hashmap)))
    hashmap))

(puthash
 "Copy Password"
 (global-interactive-emacs-candidate
  :key "Copy Password"
  :value "Copy Password"
  :next-table nil
  :preview (lambda (_) (format "Search and Copy password. %s"
                               (shell-command-to-string "rbw  unlocked")))
  :next-func #'global-interactive-password-table)
 global-interactive-emacs--candidates-table)

(provide 'global-interactive-password)
;;; global-interactive-password.el ends here

