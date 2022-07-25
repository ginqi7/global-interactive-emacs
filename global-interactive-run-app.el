(require 'global-interactive-emacs)
(add-to-list 'global-interactive-default-command (list "Run App" #'global-interactive-run-app))

(add-to-list 'global-interactive-extensions-init #'global-interactive-run-app-init)

(setq global-interactive-app-command nil)

(setq global-interactive-app-command--output nil)

(defun global-interactive-app-command--find-all-app ()
  "Find all app in directory /Applications"
  (let ((process (start-process-shell-command "list" "*list-app*" "ls -1 /Applications")))
    (set-process-filter process #'global-interactive-app-command--find-all-app-filter)
    (set-process-sentinel process #'global-interactive-app-command--find-all-app-sentinel)))


(defun remove-unuseful (str)
  "Remove some unuseful char in STR"
  (replace-regexp-in-string "[\015>=*]" "" (ansi-color-apply string)))

(defun global-interactive-app-command--find-all-app-filter (proc string)
  "Asynchronous save shell output in parameter global-interactive-app-command--output"
  (when (buffer-live-p (process-buffer proc))
    (setq global-interactive-app-command--output (concat global-interactive-app-command--output () (remove-unuseful string)))))

(defun global-interactive-app-command--find-all-app-sentinel (process signal)
  "Asynchronous save all app names in parameter global-interactive-app-command"
  (when (memq (process-status process) '(exit))
    (let* ((output (split-string global-interactive-app-command--output "\n" t " +")))
      (setq global-interactive-app-command 
            (mapcar 
             (lambda (path) (string-remove-suffix ".app" path)) 
             output)))))


(defun global-interactive-run-app ()
  "Interactively select app and open it"
  (interactive)
  
  (if global-interactive-app-command
      (lexical-let* ((candidates global-interactive-app-command)
                     (selected-item (completing-read "" candidates))
                     (command (seq-filter (lambda (command) (string= selected-item command)) global-interactive-app-command)))
        (when command
          (shell-command (format "open -a \"%s\"" (car command)))))    
    (completing-read "Warnning" '("candidates is null,Maybe there is looking for the app in the computer"))))


(defun global-interactive-run-app-init()
  (when (not global-interactive-app-command)
    (global-interactive-app-command--find-all-app))
  )

(provide 'global-interactive-run-app)
