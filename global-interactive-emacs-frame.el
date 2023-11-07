;;; global-interactive-emacs-frame.el --- Configration for global interactive emacs.  -*- lexical-binding: t; -*-




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
;; ~/.emacs.d/jin-emacs/
;; ```
;; git clone git@github.com:ginqi7/jin-emacs.git
;; ```
;; Add the downloaded directory to the load path:
;; ```
;; (add-to-list 'load-path "~/.emacs.d/jin-emacs/")
;; (require 'init-new-global-interactive-emacs)
;; ```
;;

;;; Code:

(require 'orderless)
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


(defun global-interactive-emacs--create-candidate (name comment func)
  "Create candidate for NAME COMMENT FUNC."
  (let ((candidate (make-hash-table :test 'equal)))
    (puthash 'name name candidate)
    (puthash 'comment comment candidate)
    (puthash 'func func candidate)
    candidate))

(defun global-interactive-emacs--update-candidates ()
  "Update candidates."
  (setq global-interactive-emacs--candidates nil)
  (let* ((input (global-interactive-emacs--get-input))
         (input (if input input ""))
         (input-link
          (append global-interactive-emacs--input-link
                  (split-string input "\\.")))
         (url-hashtable global-interactive-emacs--actions-table)
         (candidates
          (orderless-filter
           (car input-link)
           (hash-table-keys url-hashtable))))
    (when candidates
      (setq global-interactive-emacs--cur-action-func
            (gethash (intern (car candidates)) global-interactive-emacs--actions-func)))
    (dotimes (i (1- (length input-link)))
      (when candidates
        (setq url-hashtable
              (gethash (intern (car candidates)) url-hashtable))
        (setq candidates
              (orderless-filter
               (nth (1+ i) input-link)
               (hash-table-keys url-hashtable)))))
    (dolist (name candidates)
      (setq global-interactive-emacs--candidates
            (append global-interactive-emacs--candidates
                    (list
                     (global-interactive-emacs--create-candidate
                      name
                      (symbol-name(type-of
                                   (gethash
                                    (intern (car candidates))
                                    url-hashtable)))
                      `(lambda ()
                         (interactive)
                         (print
                          (funcall
                           ',global-interactive-emacs--cur-action-func
                           ,(gethash
                             (intern (car candidates))
                             url-hashtable)))))))))))

(defun global-interactive-emacs--reset-candidates-timer ()
  "Reset candidates timer."
  (when global-interactive-emacs--candidates-timer
    (cancel-timer global-interactive-emacs--candidates-timer))
  (setq global-interactive-emacs--candidates-timer
        (run-at-time t 1 'global-interactive-emacs-update)))

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
        (dolist (candidate global-interactive-emacs--candidates)
          (insert (gethash 'name candidate))
          (insert "   ")
          (insert (gethash 'comment candidate))
          (insert "\n"))
        (global-interactive-emacs--mark-selected-candidate)))))

(defun global-interactive-emacs--update-candidates-frame ()
  "Refresh candidates frame."
  (let* ((input-frame
          (gethash 'input global-interactive-emacs--frames))
         (input-frame-position)
         (candidates-frame
          (gethash 'candidates global-interactive-emacs--frames)))
    (when (and input-frame candidates-frame)
      (setq input-frame-position (frame-position input-frame))
      (make-frame-visible candidates-frame)
      (select-frame-set-input-focus input-frame)
      (set-frame-position candidates-frame
                          (car input-frame-position)
                          (+ (cdr input-frame-position) 30))
      (set-frame-height candidates-frame
                        (length global-interactive-emacs--candidates)))))

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
  (global-interactive-emacs--frame-new 'actions)
  )

(defun global-interactive-emacs--frame-new (name)
  "Create a frame by NAME."
  (unless (gethash name global-interactive-emacs--frames)
    (let ((frame
           (make-frame
            `((name .
                    ,(format "global-interactive-emacs-frame-%s" name))
              (no-accept-focus . nil)
              (no-focus-on-map . nil)
              (min-width . 0)
              (min-height . 0)
              (width . 60)
              (height . 0)
              (text-height . 10)
              (native-height . 10)
              (border-width . 0)
              (left-fringe . 10)
              (right-fringe . 0)
              (vertical-scroll-bars . nil)
              (horizontal-scroll-bars . nil)
              (menu-bar-lines . 0)
              (tool-bar-lines . 0)
              (tab-bar-lines . 0)
              (no-other-frame . t)
              (no-other-window . t)
              (no-delete-other-windows . t)
              (unsplittable . t)
              (undecorated . t)
              (cursor-type . nil)
              (visibility . nil)
              (minibuffer . nil)
              (no-special-glyphs . t)
              (desktop-dont-save . t)
              (mode-line-format . nil)))))
      (switch-to-buffer
       (gethash name global-interactive-emacs--buffers))
      (global-interactive-emacs-input-mode)
      (erase-buffer)
      (puthash name frame global-interactive-emacs--frames)
      frame)))

(defun global-interactive-emacs-frame ()
  "Global Interactive Emacs Frame."
  (interactive)
  (global-interactive-emacs-frame-init)
  (let ((input-frame
         (gethash 'input global-interactive-emacs--frames)))
    (make-frame-visible input-frame)
    (global-interactive-emacs--reset-candidates-timer)
    ))

(defun global-interactive-emacs-quit ()
  "Delete all global interactive Emacs frames."
  (interactive)
  (dolist (frame (hash-table-values global-interactive-emacs--frames))
    (delete-frame frame))
  (dolist (buffer (hash-table-values global-interactive-emacs--buffers))
    (kill-buffer buffer))

  (clrhash global-interactive-emacs--frames)
  (clrhash global-interactive-emacs--buffers))


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
    (global-interactive-emacs-quit)
    ))


(define-minor-mode global-interactive-emacs-input-mode
  "Global interactive Emacs Input mode."
  :keymap (let
              ((map (make-sparse-keymap)))
            (define-key map (kbd "RET") #'global-interactive-emacs-run-selected-candidate)
            (define-key map (kbd "C-g") #'global-interactive-emacs-quit)
            (define-key map
                        (kbd "C-p")
                        #'global-interactive-emacs-select-previous)
            (define-key map
                        (kbd "C-n")
                        #'global-interactive-emacs-select-next)
            map))


(provide 'global-interactive-emacs-frame)
;;; global-interactive-emacs-frame.el ends here
