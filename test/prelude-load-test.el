;;; prelude-load-test.el --- Smoke test that Prelude loads without errors -*- lexical-binding: t; -*-

;;; Commentary:

;; Loads Prelude core and all modules in batch mode, capturing any
;; errors.  Intended for CI.  Exit code is non-zero if any module
;; fails to load.
;;
;; Usage:
;;   emacs --batch -l init.el -l test/prelude-load-test.el

;;; Code:

(defvar prelude-load-test-errors nil
  "List of (module . error) pairs collected during the load test.")

(defvar prelude-load-test-modules
  '(prelude-vertico
    prelude-company
    prelude-org
    prelude-programming
    prelude-c
    prelude-clojure
    prelude-common-lisp
    prelude-css
    prelude-dart
    prelude-emacs-lisp
    prelude-elixir
    prelude-erlang
    prelude-fsharp
    prelude-go
    prelude-haskell
    prelude-js
    prelude-latex
    prelude-lisp
    prelude-lua
    prelude-ocaml
    prelude-perl
    prelude-python
    prelude-racket
    prelude-ruby
    prelude-rust
    prelude-scala
    prelude-scheme
    prelude-scss
    prelude-shell
    prelude-ts
    prelude-web
    prelude-xml
    prelude-yaml)
  "All modules to test.  Mutually exclusive modules (ido/ivy/helm)
and modules with heavy external deps (erc, evil, literate-programming)
are excluded.")

(message "\n[test] Verifying core loaded successfully...")
(unless (featurep 'prelude-editor)
  (push '(prelude-core . "core modules did not load") prelude-load-test-errors))

(message "[test] Loading all modules one by one...\n")

(dolist (mod prelude-load-test-modules)
  (condition-case err
      (progn
        (require mod)
        (message "[test]   ✓ %s" mod))
    (error
     (message "[test]   ✗ %s: %s" mod (error-message-string err))
     (push (cons mod err) prelude-load-test-errors))))

(message "")
(if prelude-load-test-errors
    (progn
      (message "[test] FAILED — %d module(s) failed to load:"
               (length prelude-load-test-errors))
      (dolist (entry (nreverse prelude-load-test-errors))
        (message "[test]   %s: %s" (car entry) (error-message-string (cdr entry))))
      (kill-emacs 1))
  (message "[test] PASSED — core and %d modules loaded successfully."
           (length prelude-load-test-modules))
  (kill-emacs 0))

;;; prelude-load-test.el ends here
