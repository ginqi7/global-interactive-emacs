(require 'global-interactive-emacs)
(add-to-list 'global-interactive-default-command (list "Web Search" #'global-interactive-web-search))

(defvar global-interactive-web-search-plist nil)

(defvar global-interactive-web-search-yaml-path nil)

(defun global-interactive-web-search-init ()
  (when (and (not global-interactive-web-search-plist)
             global-interactive-url-yaml-path)
    (require 'yaml)
    (setq global-interactive-web-search-plist (yaml-parse-string (global-interactive-file-to-string global-interactive-web-search-yaml-path)
                                                    :object-type 'plist
                                                    :sequence-type 'array
                                                    :null-object :empty))))

(defun global-interactive--keys-to-names (keys)
  (mapcar (lambda (key) (string-remove-prefix ":" key)) (mapcar #'symbol-name keys)))

(defun global-interactive--find-key (name keys)
  (car (seq-filter (lambda (key) (string= name (string-remove-prefix ":" (symbol-name key)))) keys)))

(defun global-interactive--find-value-in-vector-by-key (key the-vector)
  (plist-get (car (seq-filter (lambda (item) (eq (car item) key)) the-vector)) key))

(defun global-interactive-web-search (&optional url-expr)
  "Interactively select url and open it"
  (interactive)
  (global-interactive-web-search-init)
  (cond ((stringp url-expr) 
         (shell-command (format "open \"%s\"" 
                                (string-replace "${param}" 
                                                (completing-read "Input your search keyword: " '("Input your search keyword: ")) 
                                                url-expr))))
        ((vectorp url-expr) 
         (let* ((keys (mapcar #'car url-expr))
                        (selected-item (completing-read "Choose Url label: " (global-interactive--keys-to-names keys)))
                        (key (global-interactive--find-key selected-item keys))
                        (url-expr (global-interactive--find-value-in-vector-by-key key url-expr)))
           (when url-expr
             (global-interactive-web-search url-expr))))
        ((not url-expr) 
         (let* 
             ((url-plist global-interactive-web-search-plist)
              (keys (seq-filter #'symbolp global-interactive-web-search-plist))
              (selected-item (completing-read "Choose Url label: " (global-interactive--keys-to-names keys)))
              (key (global-interactive--find-key selected-item keys))
              (url-expr (plist-get url-plist key)))
           (when url-expr
             (global-interactive-web-search url-expr))))))

(defun global-interactive-web-search-update ()
  (interactive)
  (when global-interactive-web-search-yaml-path 
    (require 'yaml)
    (setq global-interactive-web-search-plist (yaml-parse-string (global-interactive-file-to-string global-interactive-web-search-yaml-path)
                                                          :object-type 'plist
                                                          :sequence-type 'array
                                                          :null-object :empty))))

(provide 'global-interactive-web-search)
