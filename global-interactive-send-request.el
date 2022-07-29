;;; global-interactive-send-request.el --- Global interactively send http request  -*- lexical-binding: t; -*-

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

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `global-interactive-send-request'
;;    Send request with chrome cookie.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:


(require 'global-interactive-emacs)
(add-to-list 'global-interactive-default-command (list "Send Request" #'global-interactive-send-request))

(defvar global-interactive-python-shell-path nil)

(defun global-interactive--json-to-preview(json)
  "Simplify preview JSON if is too long."
  (let* ((json-str (json-serialize json)))
    (if (> (length json-str) 40)
        (cond ((hash-table-p json) (format "%s...}" (substring json-str 0 40)))
              ((vectorp json) (format "%s...]" (substring json-str 0 40)))
              (t (format "%s..." (substring json-str 0 40))))
      json-str)))

(defun global-interactive--json-to-candidates (json)
  "Convert JSON Item to interactive candidates."
  (let (candidates)
    (cond ((hash-table-p json) (maphash (lambda (k v) (push (format "%s: %s" k (global-interactive--json-to-preview v)) candidates)) json))
          ((vectorp json)
           (let ((index -1))
             (setq candidates (mapcar (lambda (item) (setq index (+ index 1)) (format "%s:%s" index (global-interactive--json-to-preview item))) json))))
          (t (setq candidates (list json))))
    (append candidates (list global-interactive-back-to-upper-level))))

(defun global-interactive--parse-json (last-json json)
  "Parse Response JSON and interactive copy.
LAST-JSON used to go back."
  (let ((json-str (json-serialize json)))
    (when (not (string= "{}" json-str))
      (kill-new (json-serialize json))))
  
  (let* ((candidates (global-interactive--json-to-candidates json))
         (selected-item (completing-read "Response: " candidates))
         (key (car (split-string selected-item ":" t " +"))))
    (cond ((string= key global-interactive-back-to-upper-level) (global-interactive--parse-json nil last-json))
          ((hash-table-p json) (global-interactive--parse-json json (gethash key json)))
          ((vectorp json) (global-interactive--parse-json json (elt json (string-to-number key)))))))

(defun global-interactive-send-request ()
  "Send request with chrome cookie."
  (interactive)
  (if global-interactive-python-shell-path
      (let* ((method (completing-read "Selected Request Method: " '("GET" "POST")))
             (url (global-interactive-read "Please Input URL: " '("Please Input URL:")))
             (body (when (string= method "POST") (global-interactive-read "Please Input Body: " '("Please Input Body: "))))
             (command (format "python3 %s %s %s '%s'" global-interactive-python-shell-path method url body))
             (response (shell-command-to-string command)))
        (global-interactive--parse-json nil (json-parse-string response)))
    (completing-read "Warnning" '("Please config 'global-interactive-python-shell-path'"))))

(provide 'global-interactive-send-request)
;;; global-interactive-send-request.el ends here
