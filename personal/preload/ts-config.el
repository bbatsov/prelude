(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))


(use-package js
  :after (lsp-mode lsp-ui)
  :hook ((js-mode . lsp-deferred))
  :bind (:map js-mode-map
              ("C-c C-n" . lsp-rename))
  :config
  ;; TODO: what is the mode name for this?
  ;; (evil-leader/set-key-for-mode javascript-mode
  ;;   "fp" 'js-find-package-json)
  (defun esc/config-javascript-mode ()
    (define-key js-mode-map (vector 'remap 'fill-paragraph) 'javascript-c-fill-paragraph)
                                        ; (add-hook 'before-save-hook #'lsp-format-buffer t t) ;; one day, sweet prince
                                        ; (add-hook 'before-save-hook #'lsp-organize-imports t t) ;; one day, sweet prince
    (setq-local company-idle-delay 0)
    (setq-local company-minimum-prefix-length 1)
    (setq-local lsp-idle-delay 0.25)
    (setq-local lsp-ui-sideline-show-code-actions nil))
  (add-hook 'javascript-mode-hook 'esc/config-javascript-mode))
