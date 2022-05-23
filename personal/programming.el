(require 'prelude-programming)

;; Similar to vim-illuminate
(prelude-require-packages '(idle-highlight-mode))

(defun personal-prog-mode-defaults ()
  ;; Disable auto-saving features
  (setq auto-save-default nil)
  (require 'super-save)
  (super-save-mode -1)
  ;; This makes the text completion case sensitive.
  ;; https://emacs.stackexchange.com/a/10838/31688
  (setq company-dabbrev-downcase nil)
  (idle-highlight-mode +1)
  ;; Treat "_" as part of words.
  ;; https://evil.readthedocs.io/en/latest/faq.html#underscore-is-not-a-word-character
  (modify-syntax-entry ?_ "w"))

(add-hook 'prelude-prog-mode-hook 'personal-prog-mode-defaults)
