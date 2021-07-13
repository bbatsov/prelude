;; Disable auto-saving features
(setq auto-save-default nil)
(require 'super-save)
(super-save-mode -1)

;; Similar to vim-illuminate
(prelude-require-packages '(idle-highlight-mode))
(add-hook 'prelude-prog-mode-hook 'idle-highlight-mode)

;; Treat "_" as part of words.
;; https://evil.readthedocs.io/en/latest/faq.html#underscore-is-not-a-word-character
(add-hook 'prelude-prog-mode-hook
          (lambda () (modify-syntax-entry ?_ "w")))
