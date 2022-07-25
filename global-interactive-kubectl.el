(require 'global-interactive-emacs)
(require 'kubectl)
(add-to-list 'global-interactive-default-command (list "Kubectl" #'k8s-get-interactive))

(provide 'global-interactive-kubectl)
