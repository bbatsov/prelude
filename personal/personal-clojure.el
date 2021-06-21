(require 'clojure-mode)
(require 'evil)

(with-eval-after-load 'clojure-mode
  (defun personal-clojure-mode-defaults ()
    (hs-minor-mode +1))

  (evil-define-key 'normal clojure-mode-map
    ")" 'sp-end-of-next-sexp)

  (add-hook 'clojure-mode-hook 'personal-clojure-mode-defaults))
