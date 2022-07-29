(require 'global-interactive-emacs)
(add-to-list 'global-interactive-default-command (list "Run App" #'global-interactive-run-app))

(setq global-interactive-app-command nil)

(defun global-interactive-app-command--find-all-app ()
  (let* ((output (split-string (shell-command-to-string "ls -1 /Applications") "\n" t " +")))
    (setq global-interactive-app-command 
          (mapcar 
           (lambda (path) (string-remove-suffix ".app" path)) 
           output))))


(defun global-interactive-run-app ()
  "Interactively select app and open it"
  (interactive)
  (if global-interactive-app-command
      (let* ((candidates global-interactive-app-command)
                     (selected-item (completing-read "" candidates))
                     (command (seq-filter (lambda (command) (string= selected-item command)) global-interactive-app-command)))
        (when command
          (shell-command (format "open -a \"%s\"" (car command)))))    
    (progn  (completing-read "Warnning" '("candidates is null,Maybe there is looking for the app in the computer")) 
            (global-interactive-run-app-init))))


(defun global-interactive-run-app-init()
  (when (not global-interactive-app-command)
    (global-interactive-app-command--find-all-app)))


(provide 'global-interactive-run-app)
