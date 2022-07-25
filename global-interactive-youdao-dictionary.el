(require 'global-interactive-emacs)
(require 'youdao-dictionary)
(add-to-list 'global-interactive-default-command (list "Youdao Dictionary" #'global-interactive-youdao-dictionary))

(defun global-interactive-buffer-whole-string (buffer)
  "Get String without properties from other buffer"
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun global-interactive-youdao-dictionary()
  (interactive)
  (let* ((word (completing-read "Input your word: " '("Input your word: ")))
         (buffer-name "*Youdao Dictionary*")
         translated-string
         translated-value
         copy-line
         )
    (youdao-dictionary-search word)
    (setq translated-string (global-interactive-buffer-whole-string buffer-name))
    (setq translated-value (split-string translated-string "\n" t " +"))
    (delete-windows-on buffer-name)
    (kill-buffer buffer-name)
    (setq copy-line (completing-read "Select item to copy" translated-value))
    (if (string= copy-line "")
        (kill-new translated-string)
      (kill-new copy-line))))

(provide 'global-interactive-youdao-dictionary)

