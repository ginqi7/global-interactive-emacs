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

(require 'global-interactive-emacs-frame)

(defcustom global-interactive-emacs-input-separator "."
  "The separator char.")

(defclass global-interactive-emacs-candidate ()
  ((key :initarg :key)
   (value :initarg :value)
   (preview :initarg :preview)
   (next-table :initarg :next-table)
   (next-func :initarg :next-func)))

(defclass global-interactive-emacs-input ()
  ((separator :initarg :separator)
   (raw :initarg :raw)
   (type :initarg :type)
   (link :initarg :link)
   (prev :initarg :prev)
   (updated :initarg :updated)))

(cl-defmethod gie-candidate-next ((candidate global-interactive-emacs-candidate))
  (let ((table (eieio-oref candidate 'next-table)))
    (unless table
      (setq table (funcall
                   (eieio-oref candidate 'next-func)
                   (eieio-oref candidate 'value))))
    (eieio-oset candidate 'next-table table)
    table))

(cl-defmethod gie-input-update ((input global-interactive-emacs-input) raw-str)
  (let* ((prev (eieio-oref input 'raw))
         (link (split-string
                raw-str
                (concat "\\" global-interactive-emacs-input-separator)))
         (updated (not (string= prev raw-str))))
    (eieio-oset input 'raw raw-str)
    (eieio-oset input 'prev prev)
    (eieio-oset input 'updated updated)
    (eieio-oset input 'link link)
    input))

(cl-defmethod gie-candidate-to-input-buffer ((candidate global-interactive-emacs-candidate)
                                             (input global-interactive-emacs-input))
  (let ((key (eieio-oref candidate 'key))
        (link (eieio-oref input 'link))
        (raw))
    (setq link (append (butlast link) (list key)))
    (setq raw (concat
               (string-join link global-interactive-emacs-input-separator)
               global-interactive-emacs-input-separator))
    (global-interactive-emacs--insert-input raw t)))

(defvar global-interactive-emacs--candidates nil "Candidates.")
(defvar global-interactive-emacs--selected-index 0 "Selected index.")
(defvar global-interactive-emacs--selected-overlay nil "Selected overlay.")
(defvar global-interactive-emacs--last-app nil "Last front app.")

(defvar global-interactive-emacs--input
  (global-interactive-emacs-input
   :separator global-interactive-emacs-input-separator
   :raw ""
   :type ""
   :link '("")
   :prev ""
   :updated nil)
  "input.")


(defvar global-interactive-emacs--buffers
  (make-hash-table :test 'equal)
  "Buffers.")
(defvar global-interactive-emacs--frames
  (make-hash-table :test 'equal)
  "Frames.")

(defvar global-interactive-emacs--candidates-table
  (make-hash-table :test 'equal)
  "Actions table.")

(defun global-interactive-emacs-update ()
  "Update."
  (gie-input-update global-interactive-emacs--input
                    (global-interactive-emacs--get-input))
  (when (eieio-oref global-interactive-emacs--input 'updated)
    (global-interactive-emacs--update-candidates)
    (global-interactive-emacs--update-candidates-buffer)
    (global-interactive-emacs--update-candidates-frame)
    (global-interactive-emacs--update-preview-buffer)
    (global-interactive-emacs--update-preview-frame)))


(defun global-interactive-emacs--insert-input-separator ()
  "Reset intput."
  (interactive)
  (global-interactive-emacs--insert-input global-interactive-emacs-input-separator))

(defun global-interactive-emacs--insert-input (str &optional erase)
  "Insert STR intput."
  (let ((input-buffer
         (gethash 'input global-interactive-emacs--buffers)))
    (when input-buffer
      (when erase
        (erase-buffer))
      (with-current-buffer input-buffer (insert str)))))

(defun global-interactive-emacs--create-candidate (name comment func)
  "Create candidate for NAME COMMENT FUNC."
  (let ((candidate (make-hash-table :test 'equal)))
    (puthash 'name name candidate)
    (puthash 'comment comment candidate)
    (puthash 'func func candidate)
    candidate))

(defun global-interactive-emacs--extract (completions-list)
  (let ((result '())
        (current completions-list))
    (while (and (consp current)
                (not (integerp (cdr current))))
      (push (car current) result)
      (setq current (cdr current)))
    (when (consp current)
      (push (car current) result))
    (nreverse result)))

(defun global-interactive-emacs--filter (str table)
  "Filter elements by STR in TABLE."
  (mapcar (lambda (key) (gethash key table))
          (global-interactive-emacs--extract
           (completion-all-completions str (hash-table-keys table) nil nil))))


(defun global-interactive-emacs--update-candidates ()
  "Update candidates."
  (setq global-interactive-emacs--candidates nil)
  (let* ((input-link (eieio-oref
                      global-interactive-emacs--input
                      'link))
         (hashtable global-interactive-emacs--candidates-table)
         (candidates
          (global-interactive-emacs--filter
           (car input-link)
           hashtable)))
    (dotimes (i (1- (length input-link)))
      (when candidates
        (setq hashtable
              (gie-candidate-next (car candidates)))
        (when (hash-table-p hashtable)
          (setq candidates
                (global-interactive-emacs--filter
                 (nth (1+ i) input-link)
                 hashtable)))))
    (setq global-interactive-emacs--candidates candidates)))

(defun global-interactive-emacs--init-input-buffer-watcher ()
  (let ((input-buffer
         (gethash 'input global-interactive-emacs--buffers)))    
    (when input-buffer
      (with-current-buffer input-buffer
        (add-hook 'after-change-functions
                  (lambda (start end length)
                    (global-interactive-emacs-update))
                  nil t)))))

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
        (let ()
          (dolist (candidate global-interactive-emacs--candidates)
            (insert (eieio-oref candidate 'key))
            (insert "\n")))
        (global-interactive-emacs--mark-selected-candidate)))))

(defun global-interactive-emacs--update-preview-buffer ()
  "Refresh preview buffer."
  (let ((prewview-buffer
         (gethash 'preview global-interactive-emacs--buffers)))
    (when prewview-buffer
      (with-current-buffer prewview-buffer
        (erase-buffer)
        (when-let* ((selected-candidate
                     (nth global-interactive-emacs--selected-index
                          global-interactive-emacs--candidates))
                    (preview-func
                     (and (slot-boundp selected-candidate 'preview)
                          (eieio-oref selected-candidate 'preview)))
                    (arg (eieio-oref selected-candidate 'value)))
          (insert (funcall preview-func arg)))))))

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
  (when (featurep 'macos-controller) (macc-get-actived-app))
  (global-interactive-emacs-frame-init)
  (let ((input-frame
         (gethash 'input global-interactive-emacs--frames)))
    (make-frame-visible input-frame)
    (global-interactive-emacs--init-input-buffer-watcher)))

(defun global-interactive-emacs-quit-back ()
  (interactive)
  (global-interactive-emacs-quit)
  (when global-interactive-emacs--last-app
    (do-applescript (format "tell application \"%s\" to activate" global-interactive-emacs--last-app))
    (setq global-interactive-emacs--last-app nil)))

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
  (global-interactive-emacs--update-preview-buffer)
  (global-interactive-emacs--update-candidates-buffer))

(defun global-interactive-emacs-select-previous ()
  "Select previous candidate."
  (interactive)
  (setq global-interactive-emacs--selected-index
        (1- global-interactive-emacs--selected-index))
  (unless (>= global-interactive-emacs--selected-index 0)
    (setq global-interactive-emacs--selected-index
          (1- (length global-interactive-emacs--candidates))))
  (global-interactive-emacs--update-preview-buffer)
  (global-interactive-emacs--update-candidates-buffer))

(defun global-interactive-emacs-run-selected-candidate ()
  "Run selected candidate."
  (interactive)
  (when-let* ((selected-candidate
               (nth global-interactive-emacs--selected-index
                    global-interactive-emacs--candidates))
              (table (gie-candidate-next selected-candidate)))
    (if (hash-table-p table)
        (gie-candidate-to-input-buffer
         selected-candidate
         global-interactive-emacs--input)
      (global-interactive-emacs-quit))))

(define-minor-mode global-interactive-emacs-input-mode
  "Global interactive Emacs Input mode."
  :keymap (let
              ((map (make-sparse-keymap)))
            (define-key map
                        (kbd "RET")
                        #'global-interactive-emacs-run-selected-candidate)
            (define-key map
                        (kbd "<TAB>")
                        #'global-interactive-emacs-run-selected-candidate)
            (define-key map (kbd "C-g") #'global-interactive-emacs-quit-back)
            (define-key map
                        (kbd "C-p")
                        #'global-interactive-emacs-select-previous)
            (define-key map
                        (kbd "C-n")
                        #'global-interactive-emacs-select-next)
            map))

(provide 'global-interactive-emacs)
;;; global-interactive-emacs.el ends here
