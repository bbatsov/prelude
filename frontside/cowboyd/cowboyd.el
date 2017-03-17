;;; I'm the only one that uses rbenv apparently.
(prelude-require-packages '(rbenv skewer-mode))

(add-hook 'js2-mode-hook 'skewer-mode)
