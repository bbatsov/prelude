(setq default-frame-alist '((top . 1) (left . 20) (width . 132) (height . 48)))

(set-face-font 'default "Andale Mono")
(progn
  (defvar font-height)
  (if (> (display-pixel-width) 2500)
      (setq font-height 160)
    (setq font-height 140))
  (set-face-attribute 'default nil :height font-height))

(setq prelude-theme 'solarized-dark)
