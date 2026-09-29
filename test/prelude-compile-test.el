;;; prelude-compile-test.el --- Byte-compile Prelude with warnings as errors -*- lexical-binding: t; -*-

;;; Commentary:

;; Byte-compiles all of Prelude's own Emacs Lisp files and fails if
;; any of them produces a warning.  This catches renamed or removed
;; upstream functions and variables before users run into them.  The
;; compiled files are thrown away, so no stale .elc files are left
;; behind for Prelude to load.
;;
;; Every module is loaded first, so their packages get installed and
;; the compiler knows about them.
;;
;; Usage:
;;   emacs --batch -l init.el -l test/prelude-compile-test.el

;;; Code:

(defvar prelude-compile-test-modules
  (mapcar (lambda (file) (intern (file-name-base file)))
          (directory-files prelude-modules-dir nil "\\`prelude-.*\\.el\\'"))
  "All Prelude modules, including the mutually exclusive ones.")

(dolist (module prelude-compile-test-modules)
  (require module))

(defvar prelude-compile-test-files
  (append (list (expand-file-name "init.el" prelude-dir)
                (expand-file-name "early-init.el" prelude-dir))
          (directory-files prelude-core-dir t "\\.el\\'")
          (directory-files prelude-modules-dir t "\\.el\\'")
          (directory-files (expand-file-name "test" prelude-dir) t "\\.el\\'"))
  "The files to byte-compile.")

(let ((byte-compile-error-on-warn t)
      (byte-compile-dest-file-function
       (lambda (_file) (make-temp-file "prelude-compile-test" nil ".elc")))
      (failed nil))
  (dolist (file prelude-compile-test-files)
    (unless (byte-compile-file file)
      (push file failed)))
  (if failed
      (progn
        (message "[test] FAILED - %d file(s) compiled with warnings or errors:"
                 (length failed))
        (dolist (file (nreverse failed))
          (message "[test]   %s" (file-relative-name file prelude-dir)))
        (kill-emacs 1))
    (message "[test] PASSED - %d files compiled cleanly."
             (length prelude-compile-test-files))
    (kill-emacs 0)))

;;; prelude-compile-test.el ends here
