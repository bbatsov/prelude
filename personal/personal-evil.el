(prelude-require-packages '(key-chord))
(require 'key-chord)
(require 'evil)

(key-chord-mode 1)
;; Alternative escape key definition.
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;; Makes the "redo" system work.
(evil-set-undo-system 'undo-tree)

;; Collection of "evil" configurations.
(prelude-require-packages '(evil-collection))
(evil-collection-init)

;; Makes it similar to easymotion on Vim.
(evil-define-key 'normal 'global
  (kbd "SPC") 'evil-avy-goto-char-2
  "+" 'evil-window-increase-height
  "-" 'evil-window-decrease-height)

(evil-define-key 'insert 'global
  (kbd "C-x C-f") 'company-files)

;; Better folding shortcut.
(require 'hideshow)
(require 'origami)

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
    "zp" 'personal-evil-fold-focus
    "zM" 'evil-close-folds
    ;; Navigating through folds.
    "zj" 'origami-next-fold
    "zk" 'origami-previous-fold))
