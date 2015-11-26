;;; prelude-rust.el -- rust configs

;;; Commentary:
;; This package provides functionality for working in Rust.

;;; Code:
;; requires
(prelude-require-packages
 '(company company-racer racer flycheck flycheck-rust rust-mode toml-mode))

;; configs
(setq racer-cmd "racer")

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-mode))

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(provide 'prelude-rust)
;;; prelude-rust.el ends here
