(prelude-require-package 'terminal-here)
(require 'terminal-here)
(global-set-key (kbd "C-<f5>") #'terminal-here-launch)
(global-set-key (kbd "C-<f6>") #'terminal-here-project-launch)
(setq terminal-here-command-flag "-- ")
(add-hook 'term-mode-hook
          (defun my-term-mode-hook ()
            (setq bidi-paragraph-direction 'left-to-right)))
