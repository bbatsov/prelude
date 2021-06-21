(require 'evil)

(add-hook 'gfm-mode-hook 'electric-pair-local-mode)

(evil-define-key '(normal visual) gfm-mode-map
  ",b" 'markdown-insert-bold
  ",i" 'markdown-insert-italic)
