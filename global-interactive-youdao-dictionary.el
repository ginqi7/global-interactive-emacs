;;; global-interactive-youdao-dictionary.el --- Global interactively search translation by youdao  -*- lexical-binding: t; -*-

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
;;  `global-interactive-youdao-dictionary'
;;    Global interactively query translation by youdao.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:

(require 'global-interactive-emacs)
(require 'youdao-dictionary)
(add-to-list 'global-interactive-default-command
             (list "Youdao Dictionary" #'global-interactive-youdao-dictionary))

(defun global-interactive-buffer-whole-string (buffer)
  "Get String without properties from other BUFFER."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun global-interactive-youdao-dictionary()
  "Global interactively query translation by youdao."
  (interactive)
  (let* ((word
          (completing-read "Input your word: " '("Input your word: ")))
         (buffer-name "*Youdao Dictionary*")
         translated-string
         translated-value
         copy-line)
    (youdao-dictionary-search word)
    (setq translated-string
          (global-interactive-buffer-whole-string buffer-name))
    (setq translated-value
          (split-string translated-string "\n" t " +"))
    (delete-windows-on buffer-name)
    (kill-buffer buffer-name)
    (setq copy-line
          (completing-read "Select item to copy" translated-value))
    (if (string= copy-line "")
        (kill-new translated-string)
      (kill-new copy-line))))

(provide 'global-interactive-youdao-dictionary)
;;; global-interactive-youdao-dictionary.el ends here

