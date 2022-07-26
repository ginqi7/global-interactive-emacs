(require 'subr-x)
(defvar global-interactive-default-command nil)

(defvar global-interactive-extensions-init nil)

(defvar global-interactive-back-to-upper-level "Back to the upper level	↑")

(defvar global-interactive-select-from-the-clipboard "Select from the clipboard 📋")

(defun global-interactive-select-from-clipboard (selected-item &optional copy)
  (setq selected-item (if (string= selected-item global-interactive-select-from-the-clipboard)
                          (progn 
                            (setq copy t)
                            (when (not (eq (car kill-ring) (x-get-clipboard)))
                              (kill-new (x-get-clipboard)))
                            (completing-read "Select text from clipboard: " (delq nil (delete-dups kill-ring))))
                        selected-item))
  (when copy
    (kill-new selected-item))
  selected-item)

(defun global-interactive-read (prompt items)
  (global-interactive-select-from-clipboard 
   (completing-read prompt (append items (list global-interactive-select-from-the-clipboard)))))


(defun global-interactive-choose (prompt items &rest ignored)
  "Like `completing-read' but instead use dmenu.
Useful for system-wide scripts."
  (with-temp-buffer
    (thread-first
      (cond
       ((functionp items) 
        (message (format "%s" (funcall items "" nil t))))
       ((listp (car items)) (mapcar #'car items))
       (t items))
      (string-join "\n")
      string-trim
      insert)
    (shell-command-on-region
     (point-min)
     (point-max)
     (pcase system-type
       ('gnu/linux (format "rofi -dmenu -fuzzy -i -p '%s'" prompt))
       ('darwin "choose -m"))
     nil t "*global-interactive-choose error*" nil)
    (string-trim (buffer-string))))

(defun global-interactive-emacs ()
  (interactive)
  (setq completing-read-function #'global-interactive-choose)
  (lexical-let* ((candidates (mapcar #'car global-interactive-default-command))
                 (selected-item (global-interactive-read "" candidates))
                 (command (seq-filter (lambda (command) (string= selected-item (car command))) global-interactive-default-command)))
    
    (when command
      (funcall (car (cdr (car command)))))))


;; when you focus Emacs recover completing-read-function
(defun window-selection-recover-completing-read-function ()
  (setq completing-read-function #'completing-read-default))

(add-function :after after-focus-change-function #'window-selection-recover-completing-read-function)

(provide 'global-interactive-emacs)


