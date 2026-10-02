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

(ert-deftest prelude-use-package-ensure-runs-when-compiled ()
  "A byte-compiled `use-package' form still ensures its package at load time."
  (require 'use-package)
  (let* ((source (make-temp-file "prelude-ensure-test" nil ".el"
                                 ";;; -*- lexical-binding: t; -*-\n(use-package prelude-test-pkg :ensure t :defer t :no-require t)\n"))
         (compiled (byte-compile-dest-file source))
         (ensured nil))
    (unwind-protect
        (cl-letf (((symbol-function 'prelude-use-package-ensure)
                   (lambda (name &rest _) (push (list name load-in-progress) ensured))))
          (let ((byte-compile-warnings nil))
            (byte-compile-file source))
          (should-not ensured)
          (load compiled nil t)
          (should (equal ensured '((prelude-test-pkg t)))))
      (delete-file source)
      (when (file-exists-p compiled) (delete-file compiled)))))

(ert-deftest prelude-use-package-ensure-skips-built-in-packages ()
  "Built-in packages aren't tracked, so they don't get upgraded from ELPA."
  (require 'use-package)
  (let ((prelude-packages nil))
    (cl-letf (((symbol-function 'package-built-in-p)
               (lambda (pkg &rest _) (eq pkg 'builtin-pkg)))
              ((symbol-function 'package-installed-p) (lambda (&rest _) t)))
      (prelude-use-package-ensure 'builtin-pkg '(t) nil)
      (prelude-use-package-ensure 'elpa-pkg '(t) nil))
    (should (equal prelude-packages '(elpa-pkg)))))

(defmacro prelude-test-with-treesit (available &rest body)
  "Run BODY with fake tree-sitter modes, where AVAILABLE says if the grammar is.
`prelude-test-ts-mode' and `prelude-test-classic-mode' record themselves
in `used', grammar installs and prompts are recorded in `installed' and
`prompted', and installing a grammar makes it available.  BODY runs in
a buffer visiting a (nonexistent) file."
  (declare (indent 1))
  `(let ((used nil) (installed nil) (prompted nil) (grammar ,available)
         (prelude-treesit--declined nil)
         (prelude-treesit--languages nil)
         (noninteractive nil))
     (cl-letf (((symbol-function 'prelude-test-ts-mode) (lambda () (push 'ts used)))
               ((symbol-function 'prelude-test-classic-mode) (lambda () (push 'classic used)))
               ((symbol-function 'treesit-available-p) (lambda () t))
               ((symbol-function 'treesit-language-available-p) (lambda (&rest _) grammar))
               ((symbol-function 'prelude-treesit--recipe) (lambda (&rest _) t))
               ((symbol-function 'treesit-install-language-grammar)
                (lambda (lang &rest _) (push lang installed) (setq grammar t))))
       (with-temp-buffer
         (setq buffer-file-name "/nonexistent/prelude-test.x")
         (unwind-protect
             (progn ,@body)
           (setq buffer-file-name nil))))))

(defmacro prelude-test--treesit-mode (answer)
  "Run the Prelude mode function for the fake test language.
Prompts are answered with ANSWER and recorded in `prompted'."
  `(cl-letf (((symbol-function 'y-or-n-p)
              (lambda (&rest _) (push t prompted) ,answer)))
     (funcall (prelude-treesit-mode-function
               'prelude-test 'prelude-test-ts-mode 'prelude-test-classic-mode))))

(ert-deftest prelude-treesit-uses-ts-mode-when-grammar-is-available ()
  (prelude-test-with-treesit t
    (let ((prelude-treesit-auto-install 'ask))
      (prelude-test--treesit-mode nil)
      (should (equal used '(ts)))
      (should-not prompted))))

(ert-deftest prelude-treesit-installs-missing-grammar-when-accepted ()
  (prelude-test-with-treesit nil
    (let ((prelude-treesit-auto-install 'ask))
      (prelude-test--treesit-mode t)
      (should (equal installed '(prelude-test)))
      (should (equal used '(ts))))))

(ert-deftest prelude-treesit-falls-back-and-stops-asking-when-declined ()
  (prelude-test-with-treesit nil
    (let ((prelude-treesit-auto-install 'ask))
      (prelude-test--treesit-mode nil)
      (prelude-test--treesit-mode nil)
      (should (equal used '(classic classic)))
      (should (= (length prompted) 1))
      (should-not installed))))

(ert-deftest prelude-treesit-respects-auto-install-setting ()
  (prelude-test-with-treesit nil
    (let ((prelude-treesit-auto-install nil))
      (prelude-test--treesit-mode t)
      (should (equal used '(classic)))
      (should-not prompted))
    (let ((prelude-treesit-auto-install 'always))
      (prelude-test--treesit-mode nil)
      (should-not prompted)
      (should (equal installed '(prelude-test))))))

(ert-deftest prelude-treesit-never-prompts-in-batch ()
  (prelude-test-with-treesit nil
    (let ((prelude-treesit-auto-install 'ask)
          (noninteractive t))
      (prelude-test--treesit-mode t)
      (should (equal used '(classic)))
      (should-not prompted))))

(ert-deftest prelude-treesit-falls-back-without-ts-mode ()
  "A tree-sitter mode this Emacs doesn't have (e.g. on Emacs 29) isn't offered."
  (prelude-test-with-treesit nil
    (let ((prelude-treesit-auto-install 'always))
      (cl-letf (((symbol-function 'prelude-test-ts-mode) nil))
        (prelude-test--treesit-mode t))
      (should (equal used '(classic)))
      (should-not installed))))

(ert-deftest prelude-treesit-installs-fallback-package-on-demand ()
  (prelude-test-with-treesit nil
    (let ((prelude-treesit-auto-install nil)
          (packages nil))
      (cl-letf (((symbol-function 'prelude-test-classic-mode) nil)
                ((symbol-function 'prelude-package-install)
                 (lambda (pkg)
                   (push pkg packages)
                   (fset 'prelude-test-classic-mode (lambda () (push 'classic used))))))
        (funcall (prelude-treesit-mode-function
                  'prelude-test 'prelude-test-ts-mode
                  'prelude-test-classic-mode 'prelude-test-pkg)))
      (should (equal packages '(prelude-test-pkg)))
      (should (equal used '(classic)))
      (should-not prompted))))

(ert-deftest prelude-treesit-only-installs-when-visiting-a-file ()
  "No prompt when a mode function runs for a buffer without a file."
  (prelude-test-with-treesit nil
    (let ((prelude-treesit-auto-install 'ask)
          (buffer-file-name nil))
      (prelude-test--treesit-mode t)
      (should (equal used '(classic)))
      (should-not prompted)
      ;; that's not a decline, so visiting a file still asks
      (setq buffer-file-name "/nonexistent/prelude-test.x")
      (prelude-test--treesit-mode t)
      (should (equal prompted '(t))))))

(ert-deftest prelude-treesit-undefined-fallback-uses-fundamental-mode ()
  "A fallback mode that isn't installed (e.g. rust-mode) isn't called."
  (prelude-test-with-treesit nil
    (let ((prelude-treesit-auto-install nil))
      (funcall (prelude-treesit-mode-function
                'prelude-test 'prelude-test-ts-mode 'prelude-test-undefined-mode))
      (should (eq major-mode 'fundamental-mode))
      (should-not used)
      (should-not prompted))))

(ert-deftest prelude-treesit-recipe-defers-to-emacs ()
  "Prelude's recipes don't override the ones Emacs already has."
  (let ((treesit-language-source-alist '((json "emacs-pinned-recipe")))
        (prelude-treesit-language-sources '((json "prelude-recipe")
                                            (yaml "prelude-yaml-recipe"))))
    (should (equal (prelude-treesit--recipe 'json 'ignore) '(json "emacs-pinned-recipe")))
    (should (equal (prelude-treesit--recipe 'yaml 'ignore) '(yaml "prelude-yaml-recipe")))
    (should (assq 'yaml treesit-language-source-alist))))

(ert-deftest prelude-kill-region-kills-line-without-region ()
  "With no active region, \\[kill-region] kills the current line."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (goto-char (point-min))
    (forward-line 1)
    (deactivate-mark)
    (let ((kill-ring nil))
      (call-interactively #'kill-region)
      (should (equal (buffer-string) "one\nthree\n"))
      (should (equal (car kill-ring) "two\n")))))

(ert-deftest prelude-packages-activated-only-from-same-dir ()
  "Packages Emacs activated from another directory don't count."
  (let ((package--activated t)
        (package-user-dir "/nonexistent/prelude/elpa"))
    (should (prelude--packages-activated-p "/nonexistent/prelude/elpa/"))
    (should-not (prelude--packages-activated-p "/nonexistent/.emacs.d/elpa")))
  (let ((package--activated nil)
        (package-user-dir "/nonexistent/prelude/elpa"))
    (should-not (prelude--packages-activated-p "/nonexistent/prelude/elpa"))))

;;; prelude-test.el ends here
