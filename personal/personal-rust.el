(require 'prelude-rust)

(with-eval-after-load 'rust-mode
  (add-hook 'prelude-rust-mode-hook
            (lambda ()
              (hs-minor-mode +1)
              (yas-minor-mode +1))))
