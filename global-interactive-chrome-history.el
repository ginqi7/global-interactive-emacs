;;; global-interactive-chrome-history.el --- Global interactive Query Chrome History  -*- lexical-binding: t; -*-

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

(require 'global-interactive-emacs)
(require 'cl-seq)
(add-to-list 'global-interactive-default-command
             (list "Chrome History" #'global-interactive-chrome-history))
(defvar global-interactive-chrome-history-path  "~/Library/Application Support/Google/Chrome/Default/History") ;; define chrome history file path.

(defvar global-interactive-chrome-history-sql "SELECT title, last_visit_time, url FROM urls ORDER BY id DESC LIMIT %s") ;; define query chrome history SQL.

(defvar global-interactive-chrome-date-time "%m-%d %H:%M:%S") ;; define select-candidates' last visit time format.

(defvar global-interactive-chrome-limit 1000) ;; define select-candidates' max limit.

(defvar global-interactive-chrome-history-json nil) ;; define global chrome history json, need not to modify manual.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `global-interactive-chrome-history'
;;    Interavtive list and select Chrome history.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

(defun global-interactive-convert-history-time (history-time)
  "Convert Chrome history time to elisp time.
HISTORY-TIME https://stackoverflow.com/a/26233663/2999892"
  (format-time-string global-interactive-chrome-date-time (- (/ history-time  1000000) 11644473600)))

(defun global-interactive-build-chrome-history-candidate (history-item)
  "Build 'HISTORY-ITEM' to select-candidate.
'HISTORY-ITEM' is a hashmap."
  (format
   "(%s) (%s) (%s)"
   (global-interactive-convert-history-time (gethash "last_visit_time" history-item))
   (gethash "title" history-item)
   (gethash "url" history-item)))



(defun global-interactive-extract-valid-historys (history-json)
  "Remove duplicate history-item in 'HISTORY-JSON'.
'HISTORY-JSON' is json table."
  (cl-remove-duplicates history-json
                        :test (lambda
                                (x y)
                                (eq (gethash "url" x) (gethash "url" y)))))

(defun global-interactive-parse-chrome-history-output ()
  "Parse 'global-interactive-chrome-history-json', interactive select item."
  (let* ((history-json global-interactive-chrome-history-json)
         (candidates
          (mapcar #'global-interactive-build-chrome-history-candidate history-json))
         (selected-item
          (completing-read "Select history: " candidates))
         (history-item
          (seq-find
           (lambda (item)
             (string=
              (global-interactive-build-chrome-history-candidate item)
              selected-item))
           history-json)))
    (when history-item
      (shell-command
       (format "open '%s'" (gethash "url" history-item))))))

(defun global-interactive-chrome-history-update (output)
  "Update 'GLOBAL-INTERACTIVE-CHROME-HISTORY-JSON' by 'OUTPUT'."
  (when output
    (setq global-interactive-chrome-history-json
          (global-interactive-extract-valid-historys
           (json-parse-string output)))))

(defun global-interactive-chrome-history-sentinel (process signal)
  "Define a sentinel bind 'PROCESS'.
Update 'GLOBAL-INTERACTIVE-CHROME-HISTORY-JSON' when process exit.
PROCESS is current runing process.
SIGNAL is process accept signal."
  (when (memq (process-status process) '(exit))
    (let* ((output (process-get process 'output))
           (temp-file-name (process-get process 'temp-file-name)))
      (global-interactive-chrome-history-update output)
      (when temp-file-name (delete-file temp-file-name)))))

(defun global-interactive-update-chrome-history ()
  "Run sqlite3 to query Chrome history."
  (let ((tmp
         (format "/tmp/%s"
                 (make-temp-name "global-interactive-chrome-history")))
        process)
    (copy-file global-interactive-chrome-history-path tmp)
    (setq process
          (start-process-shell-command "global-interactive-chrome-history"
                                       "*global-interactive-chrome-history*"
                                       (format "sqlite3 -json '%s' \"%s\""
                                               tmp
                                               (format global-interactive-chrome-history-sql global-interactive-chrome-limit))))

    (process-put process 'temp-file-name tmp)
    (set-process-filter process #'global-interactive-output-filter)
    (set-process-sentinel process #'global-interactive-chrome-history-sentinel)))

(defun global-interactive-chrome-history ()
  "Interavtive list and select Chrome history."
  (interactive)
  (global-interactive-parse-chrome-history-output))

(run-at-time t 30 #'global-interactive-update-chrome-history)
(global-interactive-update-chrome-history)

(provide 'global-interactive-chrome-history)

(provide 'global-interactive-chrome-history)
;;; global-interactive-chrome-history.el ends here
