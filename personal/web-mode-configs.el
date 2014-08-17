;;; Code:
(eval-after-load "web-mode"
  '(progn

    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

    (defun my-web-mode-hook ()
      (setq web-mode-enable-auto-pairing nil))

    (add-hook 'web-mode-hook  'my-web-mode-hook)

    (defun sp-web-mode-is-code-context (id action context)
      (when (and (eq action 'insert)
                 (not (or (get-text-property (point) 'part-side)
                          (get-text-property (point) 'block-side))))

        t))

    (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))))

(provide 'web-mode-configs)

;;; web-mode-configs.el ends here
