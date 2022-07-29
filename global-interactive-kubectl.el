;;; global-interactive-kubectl.el --- Global interactively run kubectl to get some resource config  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Qiqi Jin

;; Author: Qiqi Jin(require 'global-interactive-emacs) <ginqi7@gmail.com>
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

(require 'kubectl)
(require 'global-interactive-emacs)
(add-to-list 'global-interactive-default-command (list "Kubectl" #'k8s-get-interactive))

(provide 'global-interactive-kubectl)
;;; global-interactive-kubectl.el ends here
