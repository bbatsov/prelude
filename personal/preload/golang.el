

(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))

;(setq prelude-theme 'solarized-dark)

;(setq prelude-whitespace nil)

(provide 'golang)
