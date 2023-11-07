;;; global-interactive-emacs.el --- Global Interactive Emacs  -*- lexical-binding: t; -*-

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
;;  `global-interactive-emacs'
;;    Global run Emacs intearactive function.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:

(require 'subr-x)
(require 'ansi-color)


(defvar global-interactive-default-command nil)

(defvar global-interactive-extensions-init nil)

(defvar global-interactive-back-to-upper-level "Back to the upper level ↑")

(defvar global-interactive-select-from-the-clipboard "Select from the clipboard 📋")

(defvar global-interactive-log-open nil)


(defun global-interactive-log (obj)
  (when global-interactive-log-open
    (print obj)))

(defun global-interactive-remove-unuseful (str)
  "Remove some unuseful char in STR."
  (replace-regexp-in-string "[\015>=*]" "" (ansi-color-apply str)))

(defun global-interactive-output-filter (proc string)
  "Process 'PROC' output filter.
'STRING' is output, filter will put the output in process properties."
  (when (buffer-live-p (process-buffer proc))
    (process-put proc 'output
                 (concat
                  (process-get proc 'output)
                  (global-interactive-remove-unuseful string)))))

(defun global-interactive-file-to-string (file)
  "File Content to string.
'FILE' filename."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun global-interactive-select-from-clipboard (selected-item &optional copy)
  "List all candidates in clipboard.
SELECTED-ITEM user selected item
COPY mark if copy the selected item"
  (setq selected-item
        (if (string= selected-item global-interactive-select-from-the-clipboard)
            (progn
              (setq copy t)
              (when (not (eq (car kill-ring) (gui-get-selection)))
                (kill-new (gui-get-selection)))
              (completing-read "Select text from clipboard: "
                               (delq nil (delete-dups kill-ring))))
          selected-item))
  (when copy (kill-new selected-item))
  selected-item)

(defun global-interactive-read (prompt items)
  "Extend 'completing-read'.
Add a candidate for 'global-interactive-select-from-the-clipboard'.
PROMPT is interactive prompt.
ITEMS is select candiates."
  (global-interactive-select-from-clipboard
   (completing-read prompt
                    (append items
                            (list global-interactive-select-from-the-clipboard)))))


(defun global-interactive-choose (prompt items &rest ignored)
  "Like `completing-read' but instead use dmenu.
Useful for system-wide scripts.
PROMPT is interactive prompt.
ITEMS is select candiates.
IGNORED is &rest."
  (with-temp-buffer
    (thread-first
      (cond
       ((functionp items)
        (message (format "%s" (funcall items "" nil t))))
       ((listp (car items))
        (mapcar #'car items))
       (t items))
      (string-join "\n")
      string-trim
      insert)
    (shell-command-on-region
     (point-min)
     (point-max)
     (pcase system-type
       ('gnu/linux
        (format "rofi -dmenu -fuzzy -i -p '%s'" prompt))
       ('darwin "choose -m"))
     nil t "*global-interactive-choose error*" nil)
    (string-trim (buffer-string))))

(defun global-interactive-emacs (&optional out-emacs)
  "Global run Emacs intearactive function.
OUT-EMACS is t run this function out of Emacs.
          is nil run this function in Emacs."
  (interactive)
  (when out-emacs
    (setq completing-read-function #'global-interactive-choose))
  (let*
      ((candidates (mapcar #'car global-interactive-default-command))
       (selected-item (global-interactive-read "Select your Global Action: " candidates))
       (command
        (seq-filter
         (lambda (command) (string= selected-item (car command)))
         global-interactive-default-command)))

    (when command
      (funcall (car (cdr (car command)))))))


(defun window-selection-recover-completing-read-function ()
  "When you focus Emacs recover 'completing-read-function'."
  (setq completing-read-function #'completing-read-default))

(add-function :after after-focus-change-function #'window-selection-recover-completing-read-function)

(provide 'global-interactive-emacs)
;;; global-interactive-emacs.el ends here
