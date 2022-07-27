(require 'global-interactive-emacs)
(add-to-list 'global-interactive-default-command (list "Find File" #'global-interactive-find-file))
(defvar global-interactive-find-file-base-directory "~/")


(defun global-interactive-parse-ls-single (file-item)
  (let* ((item-split (split-string file-item " +" t))
         (file-type (car item-split))
         (file-name (car (last item-split))))
    (when (eq (length item-split) 9)
      (list file-name
            (if (string-prefix-p "d" file-type)
                "(Directory)"
              "(File)")))))

(defun global-interactive-parse-ls-output (output-list base-path)
  (let* ((candidates (remove nil (mapcar #'global-interactive-parse-ls-single output-list)))
         (candidates (mapcar (lambda (item) 
                               (let* ((file-name (car item))
                                      (file-type (car (cdr item)))
                                      (file-type (if (string= file-name ".")
                                                     (format "(Current Directory is [%s])" base-path)
                                                   file-type)))
                                 (format "%s  %s " file-name file-type))) candidates))
         (seletcted-item (global-interactive-read "" candidates))
         (file-name (car (split-string seletcted-item "(" t " +")))
         (file-path (format "%s/%s" base-path file-name))
         )
    (if (cl-search "(Directory)" seletcted-item)
        (global-interactive-find-file file-path)
      (global-interactive-do-action-on-file file-path))))

(defun global-interactive-do-action-on-file (file-path)
  (let ((selected-item 
         (completing-read "" '("Copy File Simple Name"
                               "Copy File Full Name"
                               "Copy File Path"
                               "Copy File Content"
                               "Open file"
                               ))))
    (cond ((string= selected-item "Copy File Directory Path") (kill-new (file-name-directory file-path)))
          ((string= selected-item "Copy File Simple Name") (kill-new (car (last (split-string file-path "/" t " +")))))
          ((string= selected-item "Copy File Full Name") (kill-new file-path))
          ((string= selected-item "Copy File Content") (with-temp-buffer
                                                         (insert-file-contents file-path)
                                                         (mark-whole-buffer)
                                                         (copy-region-as-kill 1 (buffer-size))))
          ((string= selected-item "Open file") (shell-command-to-string (format "open %s" file-path))))))

(defun global-interactive-find-file (&optional file-path)
  (interactive)
  (let* ((file-path (if file-path
                        file-path
                      global-interactive-find-file-base-directory))
         (ls-output (shell-command-to-string (format "ls -la %s" file-path)))
         (output-list (split-string ls-output "\n" t " +")))
    (global-interactive-parse-ls-output output-list file-path)))
(provide 'global-interactive-find-file)
