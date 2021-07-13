(prelude-require-packages '(org-bullets org-preview-html))
(require 'org-bullets)
(require 'evil)

(with-eval-after-load 'org
  (defun personal-org-mode-defaults ()
    (org-bullets-mode +1)
    (smartparens-mode +1)
    (whitespace-mode -1)

    (evil-define-key 'normal org-mode-map
      "zk" 'org-previous-visible-heading
      "zj" 'org-next-visible-heading))

  (add-hook 'org-mode-hook
            'personal-org-mode-defaults))
