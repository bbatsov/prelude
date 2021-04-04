(prelude-require-packages '(key-chord))

(require 'prelude-evil)
(require 'key-chord)

;; Makes the "redo" system work.
(evil-set-undo-system 'undo-tree)

;; Alternative escape key definition.
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode 1)

;; Collection of "evil" configurations.
(prelude-require-packages '(evil-collection))
(evil-collection-init)

;; Makes it similar to easymotion on Vim.
(define-key evil-normal-state-map (kbd "SPC") 'avy-goto-char)

;; Shortcuts to increase/decrease window's height.
(define-key evil-normal-state-map (kbd "+") 'evil-window-increase-height)
(define-key evil-normal-state-map (kbd "-") 'evil-window-decrease-height)

;; Better folding shortcut.
(with-eval-after-load 'hideshow
  (setq hs-hide-comments-when-hiding-all nil)
  (evil-define-key 'normal hs-minor-mode-map
    "zm" 'hs-hide-level
    "zM" 'evil-close-folds))

(with-eval-after-load 'origami
  (defun personal-evil-fold-focus (buffer point)
    (interactive (list (current-buffer) (point)))
    (origami-show-only-node buffer point)
    (origami-open-node-recursively buffer point))

  (evil-define-key 'normal origami-mode-map
    ;; Open and close folds.
    "zA" 'origami-recursively-toggle-node
    "zm" 'personal-evil-fold-focus
    "zM" 'evil-close-folds
    ;; Navigating through folds.
    "zj" 'origami-next-fold
    "zk" 'origami-previous-fold))

(provide 'personal-evil)
