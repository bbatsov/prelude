(require 'prelude-clojure)
(require 'clojure-mode)
(require 'evil)
(prelude-require-packages '(flycheck-clj-kondo))

(defun personal-clojure-mode-defaults ()
  (subword-mode -1)
  (hs-minor-mode +1)
  ;; Disable JVM optimization that omits the stack trace.
  (setq cider-clojure-cli-global-options "-J-XX:-OmitStackTraceInFastThrow")

  (evil-define-key 'normal clojure-mode-map
    ")" 'sp-end-of-next-sexp)
  (modify-syntax-entry ?- "w" clojure-mode-syntax-table))

(with-eval-after-load 'clojure-mode
  (require 'flycheck-clj-kondo)
  (add-hook 'prelude-clojure-mode-hook 'personal-clojure-mode-defaults))

(with-eval-after-load 'cider
  (add-hook 'prelude-cider-repl-mode-hook 'personal-clojure-mode-defaults))
