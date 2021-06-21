(require 'prelude-org)
(require 'evil)

(prelude-require-package 'org-bullets)
(require 'org-bullets)

(defun personal-org-mode-defaults ()
  (org-bullets-mode 1)

  (evil-define-key 'normal org-mode-map
    "zk" 'org-previous-visible-heading
    "zj" 'org-next-visible-heading))

(add-hook 'org-mode-hook
          'personal-org-mode-defaults)

(provide 'personal-org)
