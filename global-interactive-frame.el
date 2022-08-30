;;; global-interactive-frame.el --- Create A Global Interactive Frame  -*- lexical-binding: t; -*-

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

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `global-interactive-frame'
;;    Create a transparent frame and run global-interactive-emacs.
;;  `global-interactive-frame-cleanup'
;;    Remove frames.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:
(require 'global-interactive-emacs)
(require 'posframe)
(require 'vertico)
(require 'vertico-posframe)

(defun global-interactive-frame()
  "Create a transparent frame and run 'global-interactive-emacs'."
  (interactive)
  (setq frame-alpha-lower-limit 0)
  (let ((frame
         (make-frame
          '((title . "global-interactive-frame")
            (name . "global-interactive-frame")
            (fullscreen . fullwidth)
            (alpha . 0)
            (undecorated . t)
            (delete-before)
            (auto-raise . t)))))
    (select-frame-set-input-focus frame)
    (global-interactive-emacs)))

(defun global-interactive-frame-cleanup ()
  "Remove frames."
  (interactive)
  (dolist (frame (frame-list))
    (dolist (parameter (frame-parameters frame))
      (when (and
             (string= (car parameter) "name")
             (string= (cdr parameter) "global-interactive-frame"))
        (delete-frame frame t)
        (lower-frame (selected-frame))))))

(run-with-idle-timer 30 t #'global-interactive-frame-cleanup)

;; when you want focus Emacs, delete the transparent frame.
(defun global-interactive-focus-frame ()
  "Focus a Emacs frame."
  (global-interactive-frame-cleanup)
  (let ((frame (car (frame-list))))
    (while (frame-parent frame)
      (setq frame (frame-parent frame)))
    (select-frame-set-input-focus frame)))


(advice-add 'keyboard-quit :before #'global-interactive-frame-cleanup)
(provide 'global-interactive-frame)

;;; global-interactive-frame.el ends here
