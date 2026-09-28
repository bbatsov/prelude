;;; prelude-test.el --- Regression tests for Prelude -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for specific Prelude behaviour.  Intended for CI.
;;
;; Usage:
;;   emacs --batch -l init.el -l test/prelude-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

(ert-deftest prelude-ocaml-mode-defaults-without-ocaml-eglot ()
  "The OCaml mode hook shouldn't fail when ocaml-eglot isn't installed."
  (require 'prelude-ocaml)
  (let ((prelude-lsp-client 'eglot))
    (cl-letf (((symbol-function 'ocaml-eglot-mode) nil)
              ((symbol-function 'prelude-lsp-enable) #'ignore))
      (with-temp-buffer
        (prelude-ocaml-mode-defaults)))))

;;; prelude-test.el ends here
