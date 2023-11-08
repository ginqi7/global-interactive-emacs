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
                          (+ 3 (cdr input-frame-position) (frame-char-height input-frame)))
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
  (global-interactive-emacs--frame-new 'actions))

(defun global-interactive-emacs--frame-new (name)
  "Create a frame by NAME."
  (unless (gethash name global-interactive-emacs--frames)
    (let ((frame
           (make-frame
            `((name .
                    ,(format "global-interactive-emacs-frame-%s" name))
                                        ;              (font . "DejaVu Sans Mono-12")
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
      (set-frame-font (font-spec :family "Roboto Mono" :size 25) nil (list frame))
      (switch-to-buffer
       (gethash name global-interactive-emacs--buffers))
      (global-interactive-emacs-input-mode)
      (erase-buffer)
      (puthash name frame global-interactive-emacs--frames)
      frame)))

(provide 'global-interactive-emacs-frame)
;;; global-interactive-emacs-frame.el ends here
