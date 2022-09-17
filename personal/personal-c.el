(require 'prelude-c)

(with-eval-after-load 'cc-mode
  (add-hook 'prelude-c-mode-common-hook
            (lambda ()
              (hs-minor-mode +1))))
