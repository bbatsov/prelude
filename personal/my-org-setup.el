(defun my-org-setup ()
  "Rameshwar's org mode stuff in addition to prelude"
  (setq-default org-hide-emphasis-markers t
                org-startup-indented t
                truncate-lines nil
                word-wrap t
                linum-mode t)
  (org-indent-mode))

(add-hook 'org-mode-hook 'my-org-setup)
