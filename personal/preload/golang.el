(global-set-key (kbd "C-c C-b") #'pop-tag-mark)

(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))


;(setq prelude-theme nil)
(setq prelude-theme 'atom-dark)

;(setq prelude-whitespace nil)

(provide 'golang)
