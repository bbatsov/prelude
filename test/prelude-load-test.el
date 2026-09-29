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

(defvar prelude-load-test-excluded-modules
  '(prelude-ido
    prelude-ivy
    prelude-helm
    prelude-helm-everywhere
    prelude-lsp-mode
    prelude-evil
    prelude-erc
    prelude-key-chord
    prelude-coffee
    prelude-literate-programming)
  "Modules left out of the load test.
Mutually exclusive modules (ido/ivy/helm, lsp-mode instead of Eglot)
and modules with heavy external deps or global side effects.")

(defvar prelude-load-test-modules
  (seq-difference
   (mapcar (lambda (file) (intern (file-name-base file)))
           (directory-files prelude-modules-dir nil "\\`prelude-.*\\.el\\'"))
   prelude-load-test-excluded-modules)
  "All modules to test, i.e. every module that isn't excluded.")

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
