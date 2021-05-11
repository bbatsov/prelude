(require 'evil)

(add-hook 'gfm-mode-hook 'electric-pair-mode-hook)

(evil-define-key '(normal visual) gfm-mode-map
  ",b" 'markdown-insert-bold
  ",i" 'markdown-insert-italic)
