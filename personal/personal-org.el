(require 'evil)
(require 'org)

(add-hook 'org-mode-hook
          (lambda ()
            ((define-key evil-normal-state-map
               (kbd "zj") 'org-next-visible-heading)
             (define-key evil-normal-state-map
               (kbd "zk") 'org-previous-visible-heading))))
