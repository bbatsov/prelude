(use-package go-mode
  :ensure t
  :pin melpa-stable
  :hook ((go-mode . lsp-deferred))
  :bind (:map go-mode-map
              ("C-c C-n" . lsp-rename))
  :config
  (setq lsp-gopls-staticcheck t
        lsp-gopls-complete-unimported t)
  (defun esc/configure-go-mode ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)
    (setq-local company-idle-delay 0)
    (setq-local company-minimum-prefix-length 1)
    (setq-local lsp-idle-delay 0.25))
  (add-hook 'go-mode-hook 'esc/configure-go-mode))

(use-package go-eldoc
  :ensure t
  :pin melpa-stable
  :after (eldoc go-mode)
  :hook ((go-mode . turn-on-eldoc-mode)
         (go-mode . go-eldoc-setup)))
