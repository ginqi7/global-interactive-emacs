(require 'global-interactive-emacs)
(require 'subr-x)
(add-to-list 'global-interactive-default-command (list "Chrome Bookmarks" #'global-interactive-chrome-bookmarks))

(defvar global-interactive-chrome-bookmarks-path 
  (cond ((eq system-type 'darwin) "~/Library/Application Support/Google/Chrome/Default/Bookmarks")
        ((eq system-type 'gnu/linux) "~/.config/google-chrome/Default/Bookmarks")))

(defvar global-interactive-chrome-bookmarks-list nil)

(defun global-interactive-chrome-bookmark-keys-p (keys)
  (and (member "date_added" keys)
       (member "guid" keys)
       (member "id" keys)
       (member "name" keys)
       (member "type" keys)
       (member "url" keys)))

(defun global-interactive-chrome-parse-json-to-list (json)
  (cond ((not json) nil)
        ((and (hash-table-p json) (global-interactive-chrome-bookmark-keys-p (hash-table-keys json)))
         (push json global-interactive-chrome-bookmarks-list))
        ((hash-table-p json) (mapcar #'global-interactive-chrome-parse-json-to-list (hash-table-values json)))
        ((vectorp json) (mapcar #'global-interactive-chrome-parse-json-to-list json))))

(defun global-interactive-chrome-bookmarks-init ()
  (when (not global-interactive-chrome-bookmarks-list)
    (global-interactive-chrome-parse-json-to-list (json-parse-string (global-interactive-file-to-string global-interactive-chrome-bookmarks-path)))))

(defun global-interactive-chrome-bookmarks ()
  (interactive)
  (global-interactive-chrome-bookmarks-init)
  (let* ((candidates (mapcar (lambda (item) (gethash "name" item)) global-interactive-chrome-bookmarks-list))
         (selected-item (completing-read "Select Bookmark" candidates))
         (url (gethash "url" (seq-find (lambda (item) (string= selected-item (gethash "name" item))) global-interactive-chrome-bookmarks-list))))
    (shell-command (format "open \"%s\"" url)))) 

(provide 'global-interactive-chrome-bookmarks)
