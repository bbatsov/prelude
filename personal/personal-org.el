(prelude-require-packages '(org-bullets
                            org-preview-html
                            yasnippet
                            yasnippet-snippets
                            org-drill))

(with-eval-after-load 'org
  (defun personal-org-mode-defaults ()
    (require 'yasnippet)
    (yas-reload-all)
    (yas-minor-mode +1)

    (require 'org-bullets)
    (org-bullets-mode +1)

    (smartparens-mode +1)
    (whitespace-mode -1)

    (require 'evil)
    (evil-define-key 'normal org-mode-map
      "zk" 'org-previous-visible-heading
      "zj" 'org-next-visible-heading))

  (add-hook 'org-mode-hook
            'personal-org-mode-defaults))
