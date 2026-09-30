;;; prelude-test.el --- Regression tests for Prelude -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for specific Prelude behaviour.  Intended for CI.
;;
;; Usage:
;;   emacs --batch -l init.el -l test/prelude-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

(declare-function prelude-ocaml-mode-defaults "prelude-ocaml")
(declare-function prelude-racket-enable-input-mode "prelude-racket")

(ert-deftest prelude-ocaml-mode-defaults-without-ocaml-eglot ()
  "The OCaml mode hook shouldn't fail when ocaml-eglot isn't installed."
  (require 'prelude-ocaml)
  (let ((prelude-lsp-client 'eglot))
    (cl-letf (((symbol-function 'ocaml-eglot-mode) nil)
              ((symbol-function 'prelude-lsp-enable) #'ignore))
      (with-temp-buffer
        (prelude-ocaml-mode-defaults)))))

(defmacro prelude-test-with-stale-archives (&rest body)
  "Run BODY with package installs failing until the archives are refreshed.
The calls made to both are recorded in order in `calls'."
  (declare (indent 0))
  `(let ((calls nil)
         (refreshed nil)
         (prelude--package-archives-refreshed nil))
     (cl-letf (((symbol-function 'package-install)
                (lambda (pkg)
                  (setq calls (append calls (list (list 'install pkg))))
                  (unless refreshed
                    (signal 'file-error (list "Not found" (symbol-name pkg))))))
               ((symbol-function 'package-refresh-contents)
                (lambda (&rest _)
                  (setq calls (append calls (list 'refresh)))
                  (setq refreshed t)))
               ((symbol-function 'package-installed-p) #'ignore))
       ,@body)))

(ert-deftest prelude-package-install-retries-after-refresh ()
  "A failed install refreshes the archives and tries again."
  (prelude-test-with-stale-archives
    (prelude-package-install 'foo)
    (should (equal calls '((install foo) refresh (install foo))))))

(ert-deftest prelude-package-install-refreshes-only-once ()
  "The archives aren't refreshed again after a refresh this session."
  (prelude-test-with-stale-archives
    (setq prelude--package-archives-refreshed t)
    (should-error (prelude-package-install 'foo) :type 'file-error)
    (should (equal calls '((install foo))))))

(ert-deftest prelude-package-install-applies-pins ()
  "Archives are re-read before installing a pinned package."
  (prelude-test-with-stale-archives
    (setq prelude--package-archives-refreshed t refreshed t)
    (let ((package-pinned-packages '((foo . "melpa-stable"))))
      (cl-letf (((symbol-function 'package-read-all-archive-contents)
                 (lambda () (setq calls (append calls (list 'read))))))
        (prelude-package-install 'foo)
        (should (equal calls '(read (install foo))))))))

(ert-deftest prelude-use-package-ensure-retries-after-refresh ()
  "`use-package' :ensure goes through `prelude-package-install'."
  (require 'use-package)
  (prelude-test-with-stale-archives
    (prelude-use-package-ensure 'foo '(t) nil)
    (should (equal calls '((install foo) refresh (install foo))))))

(ert-deftest prelude-use-package-ensure-installs-named-package ()
  "`:ensure some-package' installs that package instead of the form's name."
  (require 'use-package)
  (prelude-test-with-stale-archives
    (setq prelude--package-archives-refreshed t refreshed t)
    (prelude-use-package-ensure 'foo-mode '(foo) nil)
    (should (equal calls '((install foo))))))

(ert-deftest prelude-auto-install-installs-and-enables-mode ()
  "Visiting a matching file installs the package and enables its mode."
  (let ((auto-mode-alist nil)
        (enabled nil))
    (prelude-test-with-stale-archives
      (setq prelude--package-archives-refreshed t refreshed t)
      (cl-letf (((symbol-function 'foo-mode) (lambda () (setq enabled t))))
        (prelude-auto-install "\\.foo\\'" 'foo 'foo-mode)
        (funcall (cdr (assoc "\\.foo\\'" auto-mode-alist)))
        (should (equal calls '((install foo))))
        (should enabled)))))

(defvar prelude-ruby-mode-hook)

(ert-deftest prelude-module-keeps-user-mode-hook-functions ()
  "Loading a module doesn't drop functions already on its mode hook."
  (let ((prelude-ruby-mode-hook (list #'ignore)))
    (load "prelude-ruby" nil t)
    (should (memq #'ignore prelude-ruby-mode-hook))
    (should (memq #'prelude-ruby-mode-defaults prelude-ruby-mode-hook))))

(ert-deftest prelude-racket-input-mode-falls-back-on-older-racket-mode ()
  "Older racket-mode versions only have `racket-unicode-input-method-enable'."
  (require 'prelude-racket)
  (let ((enabled nil))
    (cl-letf (((symbol-function 'racket-input-mode) nil)
              ((symbol-function 'racket-unicode-input-method-enable)
               (lambda () (setq enabled t))))
      (prelude-racket-enable-input-mode)
      (should enabled))))

(ert-deftest prelude-use-package-ensure-strips-symbol-positions ()
  "Packages ensured while byte-compiling are installed by their bare symbol."
  (require 'use-package)
  (prelude-test-with-stale-archives
    (setq prelude--package-archives-refreshed t refreshed t)
    (let ((symbols-with-pos-enabled t))
      (prelude-use-package-ensure (position-symbol 'foo 10) '(t) nil))
    (should (eq (cadr (car calls)) 'foo))))

(ert-deftest prelude-check-module-conflicts-warns-about-each-group ()
  "Only groups with more than one enabled module are reported."
  (let ((prelude-conflicting-modules '((prelude-test-a prelude-test-b prelude-test-c)
                                       (prelude-test-d prelude-test-e)))
        (warnings nil))
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature &rest _)
                 (memq feature '(prelude-test-a prelude-test-c prelude-test-d))))
              ((symbol-function 'display-warning)
               (lambda (_type message &rest _) (push message warnings))))
      (prelude-check-module-conflicts))
    (should (= (length warnings) 1))
    (should (string-match-p "prelude-test-a, prelude-test-c" (car warnings)))))

(defmacro prelude-test-with-update-stubs (git-exit &rest body)
  "Run BODY with `prelude-update' stubbed so git exits with GIT-EXIT.
Whether Prelude got recompiled is recorded in `recompiled'."
  (declare (indent 1))
  `(let ((recompiled nil))
     (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
               ((symbol-function 'package-upgrade-all) #'ignore)
               ((symbol-function 'display-buffer) #'ignore)
               ((symbol-function 'call-process) (lambda (&rest _) ,git-exit))
               ((symbol-function 'prelude-recompile-init)
                (lambda () (setq recompiled t))))
       ,@body)))

(ert-deftest prelude-update-stops-when-git-pull-fails ()
  "A failed pull is reported and nothing gets recompiled."
  (prelude-test-with-update-stubs 1
    (should-error (prelude-update) :type 'user-error)
    (should-not recompiled)))

(ert-deftest prelude-update-keeps-default-directory ()
  "Updating doesn't change the current buffer's directory."
  (with-temp-buffer
    (setq default-directory "/tmp/")
    (prelude-test-with-update-stubs 0
      (prelude-update)
      (should recompiled)
      (should (equal default-directory "/tmp/")))))

(ert-deftest prelude-operate-on-number-keys-repeat ()
  "After \\`C-c .' the operator keys keep working without the prefix."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (insert "5")
    (goto-char (point-min))
    (execute-kbd-macro (kbd "C-c . + + *"))
    (should (equal (buffer-string) "14"))))

(ert-deftest prelude-use-package-ensure-tracks-packages ()
  "Packages ensured through `use-package' are recorded in `prelude-packages'."
  (require 'use-package)
  (let ((prelude-packages nil))
    (cl-letf (((symbol-function 'package-installed-p) (lambda (&rest _) t)))
      (prelude-use-package-ensure 'foo '(t) nil)
      (prelude-use-package-ensure 'foo-mode '(bar) nil))
    (should (equal (sort prelude-packages #'string<) '(bar foo)))))

(defun prelude-test--use-package-forms (form)
  "Return all `use-package' forms nested in FORM."
  (when (consp form)
    (append (and (eq (car form) 'use-package) (list form))
            (and (proper-list-p form)
                 (mapcan #'prelude-test--use-package-forms form)))))

(ert-deftest prelude-no-conditional-ensure ()
  "`:if' and friends don't stop `:ensure', so they mustn't be combined."
  (dolist (file (append (directory-files prelude-core-dir t "\\.el\\'")
                        (directory-files prelude-modules-dir t "\\.el\\'")))
    (with-temp-buffer
      (insert-file-contents file)
      (condition-case nil
          (while t
            (dolist (form (prelude-test--use-package-forms (read (current-buffer))))
              (when (and (memq :ensure form)
                         (seq-some (lambda (kw) (memq kw form)) '(:if :when :unless)))
                (ert-fail (format "%s: %S" (file-name-nondirectory file)
                                  (seq-take form 2))))))
        (end-of-file nil)))))

;;; prelude-test.el ends here
