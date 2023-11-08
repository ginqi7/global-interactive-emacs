;;; global-interactive-emacs.el --- Gloabl Interactive Emacs.  -*- lexical-binding: t; -*-

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
;; (require 'global-interactive-emacs)
;; ```
;;

;;; Code:

(require 'orderless)
(require 'global-interactive-emacs-frame)
(defvar global-interactive-emacs--candidates nil "Candidates.")
(defvar global-interactive-emacs--candidates-timer nil "Candidates update Timer.")
(defvar global-interactive-emacs--last-input "" "Last input.")
(defvar global-interactive-emacs--input-link nil "Input link.")
(defvar global-interactive-emacs--selected-index 0 "Selected index.")
(defvar global-interactive-emacs--selected-overlay nil "Selected overlay.")
(defvar global-interactive-emacs--buffers
  (make-hash-table :test 'equal)
  "Buffers.")
(defvar global-interactive-emacs--frames
  (make-hash-table :test 'equal)
  "Frames.")

(defvar global-interactive-emacs--actions-table
  (make-hash-table :test 'equal)
  "Actions table.")

(defvar global-interactive-emacs--actions-func
  (make-hash-table :test 'equal)
  "Action func.")

(defvar global-interactive-emacs--cur-action-func nil
  "Current action func.")

(defun global-interactive-emacs--input-update-p ()
  "Update."
  (not (string=
        (global-interactive-emacs--get-input)
        global-interactive-emacs--last-input)))


(defun global-interactive-emacs-update ()
  "Update."
  (when (global-interactive-emacs--input-update-p)
    (setq global-interactive-emacs--last-input
          (global-interactive-emacs--get-input))
    (global-interactive-emacs--update-candidates)
    (global-interactive-emacs--update-candidates-buffer)
    (global-interactive-emacs--update-candidates-frame)))

(defun global-interactive-emacs-reset-input ()
  "Reset intput."
  (interactive)
  (let ((input-buffer
         (gethash 'input global-interactive-emacs--buffers)))
    (when input-buffer
      (with-current-buffer input-buffer (erase-buffer)))))

(defun global-interactive-emacs--insert-input-seperator ()
  "Reset intput."
  (interactive)
  (let ((input-buffer
         (gethash 'input global-interactive-emacs--buffers)))
    (when input-buffer
      (with-current-buffer input-buffer (insert ".")))))



(defun global-interactive-emacs--create-candidate (name comment func)
  "Create candidate for NAME COMMENT FUNC."
  (let ((candidate (make-hash-table :test 'equal)))
    (puthash 'name name candidate)
    (puthash 'comment comment candidate)
    (puthash 'func func candidate)
    candidate))

(defun global-interactive-emacs--filter (str table)
  "Filter elements by STR in TABLE."
  (orderless-filter str (hash-table-keys table)))

(defun global-interactive-emacs--add-current-action-func (candidates)
  "Add current action func for CANDIDATES."
  (when candidates
    (setq global-interactive-emacs--cur-action-func
          (gethash
           (intern (car candidates))
           global-interactive-emacs--actions-func))))

(defun global-interactive-emacs--auto-add-seperator (candidates hashtable)
  "Auto add seperator for CANDIDATES and HASHTABLE."
  (when (and
         (= 1 (length candidates))
         (hash-table-p
          (gethash (intern (car candidates)) hashtable)))
    (global-interactive-emacs--insert-input-seperator)))

(defun global-interactive-emacs--update-show-candidates (candidates hashtable)
  "Update show CANDIDATES in HASHTABLE."
  (setq global-interactive-emacs--candidates
        (mapcar
         (lambda (name)
           (global-interactive-emacs--create-candidate
            name
            (symbol-name
             (type-of (gethash (intern (car candidates)) hashtable)))
            `(lambda ()
               (interactive)
               (print
                (funcall
                 ',global-interactive-emacs--cur-action-func
                 ,(gethash
                   (intern (car candidates))
                   hashtable))))))
         candidates)))

(defun global-interactive-emacs--update-candidates ()
  "Update candidates."
  (setq global-interactive-emacs--candidates nil)
  (let* ((input (global-interactive-emacs--get-input))
         (input (if input input ""))
         (input-link
          (append global-interactive-emacs--input-link
                  (split-string input "\\.")))
         (hashtable global-interactive-emacs--actions-table)
         (candidates
          (global-interactive-emacs--filter
           (car input-link)
           hashtable)))
    (global-interactive-emacs--add-current-action-func candidates)
    (dotimes (i (1- (length input-link)))
      (when candidates
        (when (hash-table-p hashtable)
          (setq hashtable
                (gethash (intern (car candidates)) hashtable)))
        (setq candidates
              (global-interactive-emacs--filter
               (nth (1+ i) input-link)
               hashtable))))
    (global-interactive-emacs--auto-add-seperator candidates hashtable)
    (global-interactive-emacs--update-show-candidates candidates hashtable)))

(defun global-interactive-emacs--reset-candidates-timer ()
  "Reset candidates timer."
  (when global-interactive-emacs--candidates-timer
    (cancel-timer global-interactive-emacs--candidates-timer))
  (setq global-interactive-emacs--candidates-timer
        (run-at-time t 0.5 'global-interactive-emacs-update)))

(defun global-interactive-emacs--get-input ()
  "Update candidates."
  (let ((input-buffer
         (gethash 'input global-interactive-emacs--buffers)))
    (when input-buffer
      (with-current-buffer input-buffer
        (buffer-substring-no-properties (point-min) (point-max))))))


(defun global-interactive-emacs--mark-selected-candidate ()
  "Mark selected candidate."
  (if global-interactive-emacs--selected-overlay
      (delete-overlay global-interactive-emacs--selected-overlay))
  (goto-char (point-min))
  (forward-line global-interactive-emacs--selected-index)
  (setq global-interactive-emacs--selected-overlay
        (make-overlay (line-beginning-position) (line-end-position)))
  (overlay-put
   global-interactive-emacs--selected-overlay
   'face calendar-holiday-marker))

(defun global-interactive-emacs--update-candidates-buffer ()
  "Refresh candidates buffer."
  (let ((candidates-buffer
         (gethash 'candidates global-interactive-emacs--buffers)))
    (when candidates-buffer
      (with-current-buffer candidates-buffer
        (erase-buffer)
        (let ((max-length
               (global-interactive-emacs--candidates-max-length global-interactive-emacs--candidates)))
          (dolist (candidate global-interactive-emacs--candidates)
            (insert (gethash 'name candidate))
            (dotimes (_
                      (- max-length
                         (global-interactive-emacs--candidate-length candidate)))
              (insert " "))

            (insert (gethash 'comment candidate))
            (insert "\n")))
        (global-interactive-emacs--mark-selected-candidate)))))

(defun global-interactive-emacs--candidate-length (candidate)
  "Get CANDIDATE length."
  (+
   (length (gethash 'name candidate))
   (length (gethash 'comment candidate))))

(defun global-interactive-emacs--candidates-max-length (candidates)
  "Get CANDIDATES max length."
  (apply 'max
         (mapcar
          (lambda (element)
            (+
             (global-interactive-emacs--candidate-length element)
             4))
          candidates)))


(defun global-interactive-emacs--buffer-new (name)
  "Create buffer by NAME."
  (puthash name
           (get-buffer-create
            (format "*global-interactive-emacs-%s*" name))
           global-interactive-emacs--buffers))

(defun global-interactive-emacs-frame-init ()
  "Global Interactive Emacs Frame Init."
  (setq global-interactive-emacs--selected-index 0)
  (global-interactive-emacs--buffer-new 'input)
  (global-interactive-emacs--buffer-new 'candidates)
  (global-interactive-emacs--buffer-new 'preview)
  (global-interactive-emacs--buffer-new 'actions)

  (global-interactive-emacs--frame-new 'input)
  (global-interactive-emacs--frame-new 'candidates)
  (global-interactive-emacs--frame-new 'preview)
  (global-interactive-emacs--frame-new 'actions))


(defun global-interactive-emacs ()
  "Global Interactive Emacs."
  (interactive)
  (global-interactive-emacs-frame-init)
  (let ((input-frame
         (gethash 'input global-interactive-emacs--frames)))
    (make-frame-visible input-frame)
    (global-interactive-emacs--reset-candidates-timer)))

(defun global-interactive-emacs-quit ()
  "Delete all global interactive Emacs frames."
  (interactive)
  (dolist (frame (hash-table-values global-interactive-emacs--frames))
    (delete-frame frame))
  (dolist (buffer (hash-table-values global-interactive-emacs--buffers))
    (kill-buffer buffer))

  (clrhash global-interactive-emacs--frames)
  (clrhash global-interactive-emacs--buffers)

  (when global-interactive-emacs--candidates-timer
    (cancel-timer global-interactive-emacs--candidates-timer)))


(defun global-interactive-emacs-select-next ()
  "Select next candidate."
  (interactive)
  (setq global-interactive-emacs--selected-index
        (1+ global-interactive-emacs--selected-index))
  (unless (length>
           global-interactive-emacs--candidates
           global-interactive-emacs--selected-index)
    (setq global-interactive-emacs--selected-index 0))
  (global-interactive-emacs--update-candidates-buffer))

(defun global-interactive-emacs-select-previous ()
  "Select previous candidate."
  (interactive)
  (setq global-interactive-emacs--selected-index
        (1- global-interactive-emacs--selected-index))
  (unless (> global-interactive-emacs--selected-index 0)
    (setq global-interactive-emacs--selected-index
          (1- (length global-interactive-emacs--candidates))))
  (global-interactive-emacs--update-candidates-buffer))

(defun global-interactive-emacs-run-selected-candidate ()
  "Run selected candidate."
  (interactive)
  (let ((selected-candidate
         (nth global-interactive-emacs--selected-index
              global-interactive-emacs--candidates)))
    (funcall (gethash 'func selected-candidate))
    (global-interactive-emacs-quit)))


(define-minor-mode global-interactive-emacs-input-mode
  "Global interactive Emacs Input mode."
  :keymap (let
              ((map (make-sparse-keymap)))
            (define-key map
                        (kbd "RET")
                        #'global-interactive-emacs-run-selected-candidate)
            (define-key map (kbd "C-g") #'global-interactive-emacs-quit)
            (define-key map
                        (kbd "C-p")
                        #'global-interactive-emacs-select-previous)
            (define-key map
                        (kbd "C-n")
                        #'global-interactive-emacs-select-next)
            map))

(defun global-interactive-file-to-string(file-name)
  "Convert content of FILE-NAME to string."
  (with-temp-buffer
    (insert-file-contents file-name)
    (buffer-substring-no-properties (point-min) (point-max))))

(provide 'global-interactive-emacs)
;;; global-interactive-emacs.el ends here
