(require 'global-interactive-emacs)
(add-to-list 'global-interactive-default-command (list "Send Request" #'global-interactive-send-request))

(defvar global-interactive-python-shell-path nil)




(defun global-interactive--get-fullpath (file-relative-path)
  (concat (file-name-directory (or load-file-name buffer-file-name)) file-relative-path))

(defun global-interactive--json-to-preview(json)
  "Simplify preview json if is too long"
  (let* ((json-str (json-serialize json)))
    (if (> (length json-str) 40)
        (cond ((hash-table-p json) (format "%s...}" (substring json-str 0 40)))
              ((vectorp json) (format "%s...]" (substring json-str 0 40)))
              (t (format "..." (substring json-str 0 40))))
      json-str)))


(defun global-interactive--json-to-candidates (json)
  "Convert Json Item to interactive candidates"
  (let (candidates)
    (cond ((hash-table-p json) (maphash (lambda (k v) (push (format "%s: %s" k (global-interactive--json-to-preview v)) candidates)) json))
          ((vectorp json) 
           (let ((index -1))
             (setq candidates (mapcar (lambda (item) (setq index (+ index 1)) (format "%s:%s" index (global-interactive--json-to-preview item))) json))))
          (t (setq candidates (list json))))
    (append candidates (list global-interactive-back-to-upper-level))))

(defun global-interactive--parse-json (last-json json)
  "Parse Response Json and interactive copy"
  (kill-new (json-serialize json))
  (let* ((candidates (global-interactive--json-to-candidates json))
         (selected-item (completing-read "Response: " candidates))
         (key (car (split-string selected-item ":" t " +"))))
    (cond ((string= key global-interactive-back-to-upper-level) (global-interactive--parse-json nil last-json))
          ((hash-table-p json) (global-interactive--parse-json json (gethash key json)))
          ((vectorp json) (global-interactive--parse-json json (elt json (string-to-number key)))))))

(defun global-interactive-select-from-clipboard (selected-item)
  (if (string= selected-item global-interactive-select-from-the-clipboard)
      (completing-read "Select text from clipboard: " kill-ring)
    selected-item))

(defun global-interactive-send-request ()
  "Send request with chrome cookie"
  (interactive)
  (if global-interactive-python-shell-path
      (let* ((method (completing-read "Selected Request Method: " '("GET" "POST")))
             (url (completing-read "Please Input URL: " (list "Please Input URL:" global-interactive-select-from-the-clipboard)))
             (url (global-interactive-select-from-clipboard url))
             (body (when (string= method "POST") (completing-read "Please Input Body: " (list "Please Input Body: " global-interactive-select-from-the-clipboard))))
             (body (global-interactive-select-from-clipboard body))
             (command (format "python3 %s %s %s '%s'" global-interactive-python-shell-path method url body))
             (response (shell-command-to-string command)))
        (global-interactive--parse-json nil (json-parse-string response)))
    (completing-read "Warnning" '("Please config 'global-interactive-python-shell-path'"))))

(provide 'global-interactive-send-request)
