(require 'global-interactive-emacs)
(add-to-list 'global-interactive-default-command (list "Open Url" #'global-interactive-open-url))

(defvar global-interactive-url-plist nil)

(defvar global-interactive-url-yaml-path nil)

(defun file-to-string (file)
  "File to string function"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun global-interactive-open-url-init ()
  (when (and (not global-interactive-url-plist)
             global-interactive-url-yaml-path)
    (require 'yaml)
    (setq global-interactive-url-plist (yaml-parse-string (file-to-string global-interactive-url-yaml-path)
                                                    :object-type 'plist
                                                    :sequence-type 'array
                                                    :null-object :empty))))

(defun global-interactive--keys-to-names (keys)
  (mapcar (lambda (key) (string-remove-prefix ":" key)) (mapcar #'symbol-name keys)))

(defun global-interactive--find-key (name keys)
  (car (seq-filter (lambda (key) (string= name (string-remove-prefix ":" (symbol-name key)))) keys)))

(defun global-interactive--find-value-in-vector-by-key (key the-vector)
  (plist-get (car (seq-filter (lambda (item) (eq (car item) key)) the-vector)) key))

(defun global-interactive-open-url (&optional url-expr)
  "Interactively select url and open it"
  (interactive)
  (global-interactive-open-url-init)
  (cond ((stringp url-expr) (shell-command (format "open \"%s\"" url-expr)))
        ((vectorp url-expr) 
         (let* ((keys (mapcar #'car url-expr))
                        (selected-item (completing-read "Choose Url label: " (global-interactive--keys-to-names keys)))
                        (key (global-interactive--find-key selected-item keys))
                        (url-expr (global-interactive--find-value-in-vector-by-key key url-expr))
                        )
           (when url-expr
             (message (format "fuckdsfsf:%s" url-expr))
             (global-interactive-open-url url-expr))))
        ((not url-expr) 
         (let* 
             ((url-plist global-interactive-url-plist)
              (keys (seq-filter #'symbolp global-interactive-url-plist))
              (selected-item (completing-read "Choose Url label: " (global-interactive--keys-to-names keys)))
              (key (global-interactive--find-key selected-item keys))
              (url-expr (plist-get url-plist key)))
           (when url-expr
             (global-interactive-open-url url-expr))))))



(provide 'global-interactive-open-url)
