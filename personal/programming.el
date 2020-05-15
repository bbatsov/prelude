;; Adds automatic code folding.
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Disable auto-saving features
(setq auto-save-default nil)
(require 'super-save)
(super-save-mode nil)
