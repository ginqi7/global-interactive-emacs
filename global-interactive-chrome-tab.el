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

(add-to-list 'global-interactive-default-command
             (list "Chrome Tabs" #'global-interactive-chrome-tab))


(defvar global-interactive-chrome-tab-scpt-path (expand-file-name "list-chrome-tab.scpt" (file-name-directory load-file-name)))

(defun global-interactive-chrome-tab()
  (interactive)
  (let* ((cmd
          (format "osascript '%s'" global-interactive-chrome-tab-scpt-path))
         (process
          (start-process-shell-command "chrome tabs" "*chrome tabs*" cmd)))
    (print cmd)
    (set-process-filter process #'global-interactive-chrome-tab--process-filter)
    (set-process-sentinel process #'global-interactive-chrome-tab--process-sentinel))
  )

(defun global-interactive-chrome-tab--process-sentinel (process signal)
  "Define a sentinel, when process finish, open output file.
PROCESS is current process.
SIGNAL is current signal."
  (when (memq (process-status process) '(exit signal failed))
    (let* ((output (process-get process 'output))
           (items (split-string output "\n" t " "))
           (candidate (completing-read "Select Chrome Tab: " items))
           (index (cl-position candidate items :test 'equal))
           (index (when index (+ 1 index)))
           (switch-tab
            (format "tell application \"Google Chrome\" to set active tab index of first window to %s " index))
           (active-chrome "activate application \"Google Chrome\""))
      (when index
        (do-applescript switch-tab)
        (do-applescript active-chrome))
      )))

(defun global-interactive-chrome-tab--process-filter (proc string)
  "Process 'PROC' output filter.
'STRING' is output, filter will put the output in process properties."
  (when (buffer-live-p (process-buffer proc))
    (process-put proc 'output
                 (concat
                  (process-get proc 'output)
                  (remove-unuseful string)))))


(provide 'global-interactive-chrome-tab)
;;; global-interactive-chrome-tab.el ends here
