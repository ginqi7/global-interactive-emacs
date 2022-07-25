(require 'global-interactive-emacs)
(require 'leetcode)
(add-to-list 'global-interactive-default-command (list "Leetcode" #'leetcode-interactive))

(provide 'global-interactive-leetcode)
