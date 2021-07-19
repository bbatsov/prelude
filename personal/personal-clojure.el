(require 'clojure-mode)
(require 'evil)
(prelude-require-packages '(flycheck-clj-kondo))

(with-eval-after-load 'clojure-mode
  (require 'flycheck-clj-kondo)
  (defun personal-clojure-mode-defaults ()
    (hs-minor-mode +1))

  (evil-define-key 'normal clojure-mode-map
    ")" 'sp-end-of-next-sexp)

  (add-hook 'clojure-mode-hook 'personal-clojure-mode-defaults))
