;;; prelude-test.el --- Regression tests for Prelude -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for specific Prelude behaviour.  Intended for CI.
;;
;; Usage:
;;   emacs --batch -l init.el -l test/prelude-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar apheleia-mode-alist)
(defvar go-ts-mode-map)

(declare-function prelude-apheleia-prefer-goimports "prelude-apheleia")
(declare-function prelude-fsharp-mode-defaults "prelude-fsharp")
(declare-function prelude-go-format-buffer "prelude-go")
(declare-function prelude-go-mode-defaults "prelude-go")
(declare-function prelude-java-mode-defaults "prelude-java")
(declare-function prelude-ocaml-mode-defaults "prelude-ocaml")
(declare-function prelude-racket-enable-input-mode "prelude-racket")
(declare-function prelude-rust-mode-defaults "prelude-rust")
(declare-function prelude-sh-mode-defaults "prelude-shell")

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

(defmacro prelude-test-with-companion-grammar (available recipe &rest body)
  "Run BODY for a language whose mode needs a companion grammar.
AVAILABLE is the list of grammars that are installed, out of
`prelude-test' and `prelude-test-companion'.  RECIPE says if Emacs has
a recipe for the companion.  Prompts are recorded in `prompted' and
answered with `answer'."
  (declare (indent 2))
  `(let ((available ,available)
         (answer t)
         (prelude-treesit-companion-grammars '((prelude-test prelude-test-companion)))
         (ensure-installed (symbol-function 'treesit-ensure-installed))
         (treesit-language-source-alist (when ,recipe '((prelude-test-companion "url")))))
     (prelude-test-with-treesit nil
       (cl-letf (((symbol-function 'treesit-language-available-p)
                  (lambda (lang &rest _) (memq lang available)))
                 ;; companions only count in Emacs 31+, which has this
                 ((symbol-function 'treesit-ensure-installed)
                  (or ensure-installed #'ignore))
                 ((symbol-function 'treesit-install-language-grammar)
                  (lambda (lang &rest _) (push lang installed) (push lang available)))
                 ((symbol-function 'y-or-n-p)
                  (lambda (prompt) (push prompt prompted) answer)))
         (let ((prelude-treesit-auto-install 'ask)
               (mode (prelude-treesit-mode-function
                      'prelude-test 'prelude-test-ts-mode 'prelude-test-classic-mode)))
           (ignore mode answer)
           ,@body)))))

(ert-deftest prelude-treesit-installs-companion-grammars ()
  "A mode's companion grammar is installed together with its main one."
  (prelude-test-with-companion-grammar nil t
    (funcall mode)
    (should (equal prompted
                   '("Install the tree-sitter grammars for prelude-test and prelude-test-companion? ")))
    (should (equal installed '(prelude-test-companion prelude-test)))
    (should (equal used '(ts)))))

(ert-deftest prelude-treesit-needs-the-companion-grammar-too ()
  "Without the companion the classic mode is used, as the other would fail."
  (prelude-test-with-companion-grammar '(prelude-test) t
    (setq answer nil)
    (funcall mode)
    (funcall mode)
    (should (equal prompted
                   '("Install the tree-sitter grammar for prelude-test-companion? ")))
    (should (equal used '(classic classic)))
    (setq prelude-treesit--declined nil
          answer t)
    (funcall mode)
    (should (equal installed '(prelude-test-companion)))
    (should (equal used '(ts classic classic)))))

(ert-deftest prelude-treesit-ignores-companions-emacs-has-no-recipe-for ()
  "Emacs versions without a recipe have a mode that doesn't need the grammar."
  (prelude-test-with-companion-grammar '(prelude-test) nil
    (funcall mode)
    (should-not prompted)
    (should (equal used '(ts)))))

(ert-deftest prelude-treesit-install-grammars-covers-companions ()
  (prelude-test-with-companion-grammar nil t
    (let ((inhibit-message t))
      (prelude-treesit-install-grammars))
    (should (equal installed '(prelude-test-companion prelude-test)))))

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

(ert-deftest prelude-java-starts-eglot-java-only-when-visiting-a-file ()
  "The language server isn't installed and started for non-file buffers."
  (require 'prelude-java)
  (let ((prelude-lsp-client 'eglot)
        (started nil))
    (cl-letf (((symbol-function 'eglot-java-mode)
               (lambda (&rest _) (push buffer-file-name started))))
      (with-temp-buffer
        (prelude-java-mode-defaults)
        (should-not started)
        (setq buffer-file-name "/nonexistent/Prelude.java")
        (unwind-protect
            (prelude-java-mode-defaults)
          (setq buffer-file-name nil))
        (should (equal started '("/nonexistent/Prelude.java")))))))

(ert-deftest prelude-java-mode-defaults-without-eglot-java ()
  "The Java mode hook shouldn't fail when eglot-java isn't installed."
  (require 'prelude-java)
  (let ((prelude-lsp-client 'eglot))
    (cl-letf (((symbol-function 'eglot-java-mode) nil))
      (with-temp-buffer
        (setq buffer-file-name "/nonexistent/Prelude.java")
        (unwind-protect
            (prelude-java-mode-defaults)
          (setq buffer-file-name nil))))))

(defvar eglot-java-mode-map)

(ert-deftest prelude-java-binds-test-and-run-keys-for-the-lsp-client ()
  "The same keys run tests and main classes with both LSP clients."
  (require 'prelude-java)
  (when (eq prelude-lsp-client 'eglot)
    (require 'eglot-java)
    (should (eq (lookup-key eglot-java-mode-map (kbd "C-c C-t")) 'eglot-java-run-test))
    (should (eq (lookup-key eglot-java-mode-map (kbd "C-c C-r")) 'eglot-java-run-main)))
  (let ((prelude-lsp-client 'lsp-mode))
    (cl-letf (((symbol-function 'prelude-lsp-enable) #'ignore))
      (with-temp-buffer
        (prelude-java-mode-defaults)
        (should (eq (local-key-binding (kbd "C-c C-t")) 'dap-java-run-test-method))
        (should (eq (local-key-binding (kbd "C-c C-r")) 'dap-java-debug))))))

(ert-deftest prelude-java-uses-tree-sitter-mode ()
  "`java-mode' is remapped, and Prelude knows where to get the grammar."
  (require 'prelude-java)
  (should (assq 'java-mode major-mode-remap-alist))
  (should (assq 'java prelude-treesit-language-sources)))

(ert-deftest prelude-auto-install-covers-gradle-build-files ()
  "Both flavours of Gradle build files get a major mode on demand."
  (should (equal (assoc-default "build.gradle" prelude-auto-install-alist #'string-match-p)
                 '(groovy-mode groovy-mode)))
  (should (equal (assoc-default "build.gradle.kts" prelude-auto-install-alist #'string-match-p)
                 '(kotlin-mode kotlin-mode))))

(defun prelude-test--lsp-enable (programs &optional installed quiet)
  "Call `prelude-lsp-enable' in a buffer of a made up `prelude-test-mode'.
PROGRAMS is the value of `eglot-server-programs' and INSTALLED the list
of programs that are installed.  QUIET is passed on.  Return a list of
whether Eglot got started and what was reported."
  (require 'eglot)
  (let ((eglot-server-programs programs)
        (prelude-lsp-client 'eglot)
        (started nil)
        (reported nil))
    (cl-letf (((symbol-function 'eglot-ensure) (lambda () (setq started t)))
              ((symbol-function 'executable-find)
               (lambda (program &rest _) (car (member program installed))))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq reported (apply #'format-message format-string args)))))
      (with-temp-buffer
        (setq major-mode 'prelude-test-mode
              buffer-file-name "/nonexistent/prelude.test")
        (unwind-protect
            (prelude-lsp-enable quiet)
          (setq buffer-file-name nil))))
    (list started reported)))

(defun prelude-test-install-and-run-server (&rest _)
  "Stand in for a package that installs a language server on demand."
  (error "The server isn't installed yet"))

(put 'prelude-test-mode 'derived-mode-parent 'prelude-test-parent-mode)

(ert-deftest prelude-lsp-enable-starts-eglot-when-the-server-is-installed ()
  (let ((prelude-lsp--reported-modes nil))
    (dolist (programs `(((prelude-test-mode "server" "--stdio"))
                        (((other-mode prelude-test-mode) "server"))
                        (((prelude-test-mode :language-id "test") "server"))
                        (((other-mode (prelude-test-mode :language-id "test")) "server"))
                        ((prelude-test-mode some-server-class "server"))
                        ((prelude-test-parent-mode "server"))
                        ((prelude-test-mode . ,(lambda (&rest _) '("server"))))
                        ((prelude-test-mode . ,(lambda (_interactive) '("server"))))
                        ((prelude-test-mode . ,(lambda (_interactive project)
                                                 (and project '("server")))))
                        ;; things we can't check without starting the server
                        ((prelude-test-mode "localhost" 7777))
                        ((prelude-test-mode . prelude-test-install-and-run-server))
                        ((prelude-test-mode . ,(lambda (&rest _) (signal 'wrong-type-argument nil))))))
      (should (equal (prelude-test--lsp-enable programs '("server"))
                     '(t nil))))))

(ert-deftest prelude-lsp-enable-reports-a-missing-server-once ()
  (dolist (programs `(((prelude-test-mode "server" "--stdio"))
                      ((prelude-test-mode some-server-class "server"))
                      ((prelude-test-mode . ,(lambda (&rest _) (error "None of them are installed"))))
                      ((other-mode "other-server"))))
    (let ((prelude-lsp--reported-modes nil))
      (pcase-let ((`(,started ,reported) (prelude-test--lsp-enable programs '("other-server"))))
        (should-not started)
        (should (string-prefix-p "[Prelude] No language server" reported)))
      (should (equal (prelude-test--lsp-enable programs '("other-server"))
                     '(nil nil))))))

(ert-deftest prelude-lsp-enable-can-keep-quiet-about-a-missing-server ()
  (let ((prelude-lsp--reported-modes nil)
        (programs '((prelude-test-mode "server"))))
    (should (equal (prelude-test--lsp-enable programs nil 'quiet) '(nil nil)))
    (should-not prelude-lsp--reported-modes)
    (should (equal (prelude-test--lsp-enable programs '("server") 'quiet) '(t nil)))))

(ert-deftest prelude-lsp-enable-keeps-lsp-mode-quiet-as-well ()
  "lsp-mode doesn't offer to install a server in buffers that asked for quiet."
  (defvar lsp-enable-suggest-server-download)
  (defvar lsp-warn-no-matched-clients)
  (let ((prelude-lsp-client 'lsp-mode)
        (lsp-enable-suggest-server-download t)
        (lsp-warn-no-matched-clients t))
    ;; no need to load (and install) lsp-mode for this
    (cl-letf (((symbol-value 'features) (cons 'prelude-lsp-mode features))
              ((symbol-function 'lsp-deferred) #'ignore))
      (with-temp-buffer
        (prelude-lsp-enable)
        (should lsp-enable-suggest-server-download)
        (should lsp-warn-no-matched-clients))
      (with-temp-buffer
        (prelude-lsp-enable 'quiet)
        (should-not lsp-enable-suggest-server-download)
        (should-not lsp-warn-no-matched-clients))
      (should lsp-warn-no-matched-clients))))

(ert-deftest prelude-lsp-enable-leaves-remote-files-to-eglot ()
  (let ((prelude-lsp--reported-modes nil)
        (default-directory "/ssh:prelude-test-host:/tmp/"))
    (cl-letf (((symbol-function 'file-remote-p) (lambda (file &rest _) (string-prefix-p "/ssh:" file))))
      (should (equal (prelude-test--lsp-enable '((prelude-test-mode "server")))
                     '(t nil))))))

(ert-deftest prelude-flycheck-eglot-works-with-the-eglot-of-emacs-29 ()
  "flycheck-eglot needs a function that Eglot has had only since Emacs 30."
  (require 'prelude-programming)
  (require 'eglot)
  (when (eq prelude-lsp-client 'eglot)
    (should (featurep 'flycheck-eglot))
    (should (fboundp 'eglot-server-capable))))

(ert-deftest prelude-lsp-enable-needs-a-file ()
  (let ((prelude-lsp-client 'eglot))
    (cl-letf (((symbol-function 'eglot-ensure) (lambda () (error "Started"))))
      (with-temp-buffer
        (prelude-lsp-enable)))))

(defun prelude-test--visit (file)
  "Return the major mode used for FILE, and whether LSP got started.
That's t, or `quiet' if a missing language server isn't to be reported.
The file isn't created, and missing tree-sitter grammars aren't
installed."
  (let ((started nil)
        (prelude-treesit-auto-install nil))
    (cl-letf (((symbol-function 'prelude-lsp-enable)
               (lambda (&optional quiet) (setq started (if quiet 'quiet t)))))
      (with-temp-buffer
        ;; some packages look for a project around the file, and not
        ;; all of them cope with a directory that doesn't exist
        (setq buffer-file-name (expand-file-name file temporary-file-directory))
        (unwind-protect
            (progn (set-auto-mode)
                   (list major-mode started))
          (setq buffer-file-name nil))))))

(ert-deftest prelude-csharp-sets-up-csharp-files ()
  (require 'prelude-csharp)
  (pcase-let ((`(,mode ,lsp) (prelude-test--visit "Prelude.cs")))
    (should (memq mode '(csharp-mode csharp-ts-mode)))
    (should lsp)
    (should (assq 'c-sharp prelude-treesit-language-sources))))

(ert-deftest prelude-julia-sets-up-julia-files ()
  (require 'prelude-julia)
  (should (equal (prelude-test--visit "prelude.jl") '(julia-mode t))))

(ert-deftest prelude-kotlin-sets-up-kotlin-files ()
  (require 'prelude-kotlin)
  (dolist (file '("Prelude.kt" "build.gradle.kts"))
    (should (equal (prelude-test--visit file) '(kotlin-mode t)))))

(ert-deftest prelude-nix-sets-up-nix-files ()
  (require 'prelude-nix)
  (should (equal (prelude-test--visit "flake.nix") '(nix-mode t))))

(ert-deftest prelude-php-sets-up-php-files ()
  (require 'prelude-php)
  (should (equal (prelude-test--visit "prelude.php") '(php-mode t))))

(ert-deftest prelude-r-sets-up-r-files ()
  (require 'prelude-r)
  (should (equal (prelude-test--visit "prelude.R") '(ess-r-mode t))))

(ert-deftest prelude-sql-sets-up-sql-files ()
  (require 'prelude-sql)
  (should (equal (prelude-test--visit "prelude.sql") '(sql-mode quiet))))

(ert-deftest prelude-config-languages-start-lsp-quietly ()
  "Shell scripts, CSS, YAML and JSON get LSP only if a server is around."
  (dolist (module '(prelude-shell prelude-css prelude-yaml prelude-js))
    (require module))
  (pcase-dolist (`(,file . ,modes) '(("prelude.sh" sh-mode bash-ts-mode)
                                     ("prelude.css" css-mode css-ts-mode)
                                     ("prelude.yml" yaml-mode yaml-ts-mode)
                                     ("prelude.json" js-json-mode json-ts-mode)))
    (pcase-let ((`(,mode ,lsp) (prelude-test--visit file)))
      (should (memq mode modes))
      (should (eq lsp 'quiet))))
  (should (equal (cdr (prelude-test--visit "prelude.js")) '(t))))

(ert-deftest prelude-tree-sitter-remaps-cover-the-modes-files-open-in ()
  "The mode a file is associated with is the one Prelude remaps.
Some of those are aliases (e.g. `javascript-mode'), which the remapping
doesn't follow."
  (dolist (module '(prelude-c prelude-csharp prelude-css prelude-go prelude-java
                              prelude-js prelude-python prelude-ruby prelude-shell))
    (require module))
  (dolist (file '("prelude.c" "prelude.h" "prelude.cpp" "Prelude.cs" "prelude.css"
                  "prelude.go" "Prelude.java" "prelude.js" "prelude.jsx"
                  "prelude.mjs" "prelude.cjs" "prelude.json" "prelude.py"
                  "prelude.rb" "prelude.gemspec" "prelude.sh"))
    (let ((mode (assoc-default file auto-mode-alist #'string-match-p)))
      (should mode)
      ;; the tree-sitter modes of older Emacsen associate themselves
      ;; with their files once their grammar is installed
      (should (or (eq mode 'c-or-c++-mode)
                  (string-suffix-p "-ts-mode" (symbol-name mode))
                  (string-prefix-p "prelude-treesit-"
                                   (symbol-name (alist-get mode major-mode-remap-alist))))))))

(ert-deftest prelude-elixir-sets-up-heex-templates-where-possible ()
  "HEEx templates use the built-in mode of Emacs 30+, and nothing gets installed."
  (require 'prelude-elixir)
  (should-not (memq 'heex-ts-mode package-selected-packages))
  (when (fboundp 'heex-ts-mode)
    (dolist (file '("index.html.heex" "index.html.eex" "index.html.leex"))
      (should (eq (assoc-default file auto-mode-alist #'string-match-p)
                  'prelude-treesit-heex-ts-mode)))))

(ert-deftest prelude-json-modes-get-the-same-setup ()
  "Both JSON modes get the JSON defaults, and only those."
  (require 'prelude-js)
  (let ((calls nil))
    (cl-letf (((symbol-function 'prelude-lsp-enable)
               (lambda (&optional quiet) (push (if quiet 'quiet t) calls))))
      (with-temp-buffer
        (js-json-mode)
        (should subword-mode)
        (should (equal calls '(quiet))))
      (setq calls nil)
      ;; the grammar might be missing, but the hook is there regardless
      (with-temp-buffer
        (run-hooks 'json-ts-mode-hook)
        (should subword-mode)
        (should (equal calls '(quiet)))))))

(ert-deftest prelude-shell-starts-lsp-for-sh-and-bash-only ()
  (require 'prelude-shell)
  (defvar sh-shell)
  (pcase-dolist (`(,shell . ,expected) '((bash . (quiet)) (sh . (quiet)) (zsh . nil) (fish . nil)))
    (let ((calls nil))
      (cl-letf (((symbol-function 'prelude-lsp-enable)
                 (lambda (&optional quiet) (push (if quiet 'quiet t) calls))))
        (with-temp-buffer
          (setq-local sh-shell shell)
          (prelude-sh-mode-defaults)
          (should (equal calls expected)))))))

(ert-deftest prelude-prog-mode-hook-runs-once ()
  "Modules don't run the hook themselves in modes derived from `prog-mode'."
  (dolist (module '(prelude-css prelude-web))
    (require module))
  (let* ((runs 0)
         (count (lambda () (setq runs (1+ runs)))))
    (add-hook 'prelude-prog-mode-hook count)
    (unwind-protect
        (cl-letf (((symbol-function 'prelude-lsp-enable) #'ignore))
          (dolist (mode '(css-mode web-mode))
            (setq runs 0)
            (with-temp-buffer
              (funcall mode))
            (should (= runs 1))))
      (remove-hook 'prelude-prog-mode-hook count))))

(defun prelude-test--hook-runs-p (mode-hook prelude-hook)
  "Return non-nil if running MODE-HOOK runs PRELUDE-HOOK."
  (let* ((ran nil)
         (record (lambda () (setq ran t))))
    (add-hook prelude-hook record)
    (unwind-protect
        (cl-letf (((symbol-function 'prelude-lsp-enable) #'ignore))
          (with-temp-buffer
            (run-hooks mode-hook)))
      (remove-hook prelude-hook record))
    ran))

(ert-deftest prelude-modules-run-their-hook-from-every-mode-they-set-up ()
  (dolist (module '(prelude-ocaml prelude-scss prelude-ts))
    (require module))
  (pcase-dolist (`(,mode-hook . ,prelude-hook)
                 '((neocaml-base-mode-hook . prelude-ocaml-mode-hook)
                   (scss-mode-hook . prelude-scss-mode-hook)
                   (typescript-ts-mode-hook . prelude-ts-mode-hook)
                   (tsx-ts-mode-hook . prelude-ts-mode-hook)
                   (typescript-mode-hook . prelude-ts-mode-hook)))
    (should (prelude-test--hook-runs-p mode-hook prelude-hook)))
  (should (memq 'prelude-ocaml-mode-defaults (symbol-value 'prelude-ocaml-mode-hook))))

(ert-deftest prelude-scss-uses-the-built-in-mode ()
  "SCSS files open in the `scss-mode' that comes with Emacs.
The scss-mode package from MELPA, which Prelude used to install, fails
to load and would leave them in `fundamental-mode'."
  (require 'prelude-scss)
  (cl-letf (((symbol-function 'prelude-lsp-enable) #'ignore))
    (with-temp-buffer
      (scss-mode)
      (should (eq major-mode 'scss-mode))
      (should (derived-mode-p 'css-mode))
      (should (equal (file-name-base (symbol-file 'scss-mode 'defun)) "css-mode"))
      ;; the CSS defaults
      (should (bound-and-true-p rainbow-mode)))))

(ert-deftest prelude-fsharp-mode-defaults-without-eglot-fsharp ()
  "The F# mode hook shouldn't fail when eglot-fsharp isn't installed."
  (require 'prelude-fsharp)
  (let ((prelude-lsp-client 'eglot)
        (load-path (seq-remove (lambda (dir) (string-match-p "eglot-fsharp" dir))
                               load-path)))
    (cl-letf (((symbol-value 'features) (remq 'eglot-fsharp features))
              ((symbol-function 'prelude-lsp-enable) #'ignore))
      (with-temp-buffer
        (prelude-fsharp-mode-defaults)
        (should subword-mode)))))

(ert-deftest prelude-terraform-sets-up-terraform-files ()
  (require 'prelude-terraform)
  (should (equal (prelude-test--visit "main.tf") '(terraform-mode t))))

(ert-deftest prelude-zig-sets-up-zig-files ()
  (require 'prelude-zig)
  (should (equal (prelude-test--visit "prelude.zig") '(zig-mode t))))

(ert-deftest prelude-make-script-executable-ignores-rust-attributes ()
  "Scripts are made executable on save, Rust files with #![...] aren't."
  (pcase-dolist (`(,contents . ,executable) '(("#!/bin/sh\necho hi\n" . t)
                                              ("#![allow(dead_code)]\n" . nil)
                                              ("echo hi\n" . nil)))
    (let ((file (make-temp-file "prelude-test")))
      (unwind-protect
          (with-temp-buffer
            (insert contents)
            (write-region nil nil file nil 'silent)
            (setq buffer-file-name file)
            (prelude-make-script-executable)
            (setq buffer-file-name nil)
            (should (eq (file-executable-p file) executable)))
        (delete-file file)))))

(defmacro prelude-test-with-formatters (&rest body)
  "Run BODY with the formatting commands recording themselves in `calls'."
  (declare (indent 0))
  `(let ((calls nil))
     (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
               ((symbol-function 'eglot-format-buffer) (lambda () (push 'eglot calls)))
               ((symbol-function 'gofmt) (lambda () (push 'gofmt calls))))
       ,@body)))

(ert-deftest prelude-format-on-save-can-be-turned-off ()
  "Modules format on save unless that's turned off or Apheleia does it."
  (require 'prelude-go)
  (defvar apheleia-mode)
  (prelude-test-with-formatters
    (prelude-lsp-format-on-save)
    (prelude-go-format-buffer)
    (should (equal calls '(gofmt eglot)))
    (setq calls nil)
    (let ((prelude-format-on-save nil))
      (prelude-lsp-format-on-save)
      (prelude-go-format-buffer))
    (let ((apheleia-mode t))
      (prelude-lsp-format-on-save)
      (prelude-go-format-buffer))
    (should-not calls)))

(ert-deftest prelude-lsp-format-on-save-never-blocks-saving ()
  "A language server that fails to format doesn't signal an error."
  (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
            ((symbol-function 'eglot-format-buffer) (lambda () (error "Boom"))))
    ;; ERT turns on `debug-on-error' in Emacs 29, which would defeat
    ;; `with-demoted-errors'
    (let ((debug-on-error nil)
          (inhibit-message t))
      (prelude-lsp-format-on-save))))

(ert-deftest prelude-lsp-format-on-save-uses-the-client-of-the-buffer ()
  "The buffer is formatted by lsp-mode when that manages it, or not at all."
  (defvar lsp-mode)
  (prelude-test-with-formatters
    (cl-letf (((symbol-function 'eglot-managed-p) #'ignore)
              ((symbol-function 'lsp-format-buffer) (lambda () (push 'lsp-mode calls))))
      (prelude-lsp-format-on-save)
      (should-not calls)
      (let ((lsp-mode t))
        (prelude-lsp-format-on-save))
      (should (equal calls '(lsp-mode))))))

(ert-deftest prelude-apheleia-prefers-goimports-when-installed ()
  (require 'prelude-apheleia)
  (dolist (goimports '(nil "/usr/bin/goimports"))
    (let ((apheleia-mode-alist (list (cons 'go-mode 'gofmt) (cons 'go-ts-mode 'gofmt))))
      (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) goimports)))
        (prelude-apheleia-prefer-goimports))
      (should (equal apheleia-mode-alist
                     (if goimports
                         '((go-mode . goimports) (go-ts-mode . goimports))
                       '((go-mode . gofmt) (go-ts-mode . gofmt))))))))

(defun prelude-test--super-save-p (mode)
  "Return non-nil if super-save would save a modified file buffer in MODE."
  (let ((file (make-temp-file "prelude-test")))
    (unwind-protect
        (with-temp-buffer
          (set-visited-file-name file t)
          (setq major-mode mode)
          (insert "modified")
          (prog1 (seq-every-p #'funcall super-save-predicates)
            (set-buffer-modified-p nil)))
      (delete-file file))))

(ert-deftest prelude-go-works-the-same-in-both-go-modes ()
  "The key bindings, formatting and super-save setup don't depend on the mode."
  (require 'prelude-go)
  (should (fboundp 'gofmt))
  (should (fboundp 'godoc-at-point))
  (require 'go-mode)
  (require 'go-ts-mode)
  (dolist (map (list go-mode-map go-ts-mode-map))
    (should (eq (lookup-key map (kbd "C-c a")) 'go-test-current-project))
    (should (eq (lookup-key map (kbd "C-c .")) 'go-test-current-test))
    (should (eq (lookup-key map (kbd "C-c b")) 'go-run)))
  (dolist (mode '(go-mode go-ts-mode))
    (should-not (prelude-test--super-save-p mode)))
  (should (prelude-test--super-save-p 'fundamental-mode))
  (with-temp-buffer
    (cl-letf (((symbol-function 'prelude-lsp-enable) #'ignore))
      (prelude-go-mode-defaults))
    (should (memq 'prelude-go-format-buffer before-save-hook))))

(ert-deftest prelude-rust-formats-on-save-and-leaves-global-hooks-alone ()
  (require 'prelude-rust)
  (should-not (prelude-test--super-save-p 'rust-ts-mode))
  (let ((after-save (default-value 'after-save-hook)))
    (with-temp-buffer
      (cl-letf (((symbol-function 'prelude-lsp-enable) #'ignore))
        (prelude-rust-mode-defaults))
      (should (memq 'prelude-lsp-format-on-save before-save-hook)))
    (should (memq 'prelude-make-script-executable after-save))
    (should (equal (default-value 'after-save-hook) after-save))))

(ert-deftest prelude-kill-region-kills-rectangles ()
  "Our advice doesn't get in the way of killing a rectangular region."
  (with-temp-buffer
    (insert "abcdef\nabcdef\nabcdef\n")
    (goto-char 2)
    (let ((transient-mark-mode t)
          (kill-ring nil))
      (rectangle-mark-mode 1)
      (forward-line 2)
      (forward-char 3)
      (call-interactively #'kill-region))
    (should (equal (buffer-string) "adef\nadef\nadef\n"))))

(put 'prelude-test-python-mode 'derived-mode-parent 'python-base-mode)

(put 'python-base-mode 'derived-mode-parent 'prog-mode)

(ert-deftest prelude-yank-leaves-indentation-sensitive-code-alone ()
  "Yanked code is re-indented, except where the indentation carries meaning."
  (pcase-dolist (`(,mode . ,indented) '((emacs-lisp-mode . t)
                                        (python-base-mode . nil)
                                        ;; e.g. `python-ts-mode'
                                        (prelude-test-python-mode . nil)
                                        (text-mode . nil)))
    (let ((called nil))
      (cl-letf (((symbol-function 'prelude-yank-indent-function)
                 (lambda (&rest _) (setq called t))))
        (with-temp-buffer
          (setq major-mode mode)
          (insert "  yanked")
          (set-mark (point-min))
          (prelude-yank-indent-advice)))
      (should (eq called indented)))))

(ert-deftest prelude-welcome-screen-stays-out-of-the-way-of-files ()
  "Starting Emacs with a file (new or not) shows the file, not the welcome screen."
  (cl-letf (((symbol-function 'prelude-welcome--render) (lambda () 'welcome)))
    (with-temp-buffer
      (let ((prelude-welcome--startup-done nil))
        (should (eq (prelude-welcome--initial-buffer) 'welcome))
        (setq buffer-file-name "/nonexistent/new-file.txt")
        (unwind-protect
            (progn
              (should (eq (prelude-welcome--initial-buffer) (current-buffer)))
              ;; later frames (e.g. of emacsclient) get the welcome screen
              (setq prelude-welcome--startup-done t)
              (should (eq (prelude-welcome--initial-buffer) 'welcome)))
          (setq buffer-file-name nil))))))

(ert-deftest prelude-delete-init-bytecode-removes-compiled-init-files ()
  "Compiled init files are deleted, and the outdated ones reported."
  (let ((prelude-dir (file-name-as-directory (make-temp-file "prelude-test" t))))
    (unwind-protect
        (cl-flet ((touch (name age)
                    (let ((file (expand-file-name name prelude-dir)))
                      (write-region "" nil file nil 'silent)
                      (set-file-times file (time-subtract nil age))
                      file)))
          (touch "init.el" 100)
          (touch "early-init.el" 100)
          (touch "other.elc" 200)
          (let ((stale (touch "early-init.elc" 200)))
            (touch "init.elc" 50)
            (should (equal (prelude-delete-init-bytecode) (list stale))))
          (should (equal (directory-files prelude-dir nil "\\.elc\\'") '("other.elc")))
          (should-not (prelude-delete-init-bytecode)))
      (delete-directory prelude-dir t))))

(ert-deftest prelude-delete-init-bytecode-survives-a-read-only-install ()
  "Not being allowed to delete the files is reported, but isn't an error."
  (let ((prelude-dir (file-name-as-directory (make-temp-file "prelude-test" t)))
        (warned nil))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "init.elc" prelude-dir) nil 'silent)
          (cl-letf (((symbol-function 'delete-file)
                     (lambda (file &rest _) (signal 'permission-denied (list file))))
                    ((symbol-function 'display-warning)
                     (lambda (&rest _) (setq warned t))))
            (prelude-delete-init-bytecode))
          (should warned))
      (delete-directory prelude-dir t))))

(ert-deftest prelude-recompile-init-leaves-the-init-files-alone ()
  "Only the directories whose code is loaded with `require' get compiled."
  (let ((compiled nil))
    (cl-letf (((symbol-function 'byte-recompile-directory)
               (lambda (dir &rest _) (push dir compiled)))
              ((symbol-function 'prelude-delete-init-bytecode) #'ignore))
      (prelude-recompile-init))
    (should (member prelude-core-dir compiled))
    (should (member prelude-modules-dir compiled))
    (should-not (member prelude-dir compiled))
    (should-not (member prelude-personal-dir compiled))))

(ert-deftest prelude-leaves-fonts-alone ()
  "Fonts are the user's business: nothing in the core sets them."
  (dolist (file (directory-files prelude-core-dir t "\\.el\\'"))
    (with-temp-buffer
      (insert-file-contents file)
      (should-not (re-search-forward
                   "(set-fontset-font\\|(set-frame-font\\|(set-face-attribute 'default"
                   nil t)))))

;;; The module listings

(defun prelude-test--file-string (file)
  "Return the contents of FILE, which is relative to `prelude-dir'."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file prelude-dir))
    (buffer-string)))

(defun prelude-test--matches (regexp string)
  "Return what the first group of REGEXP matched in STRING, every time."
  (let ((matches nil)
        (start 0))
    (while (string-match regexp string start)
      (push (match-string 1 string) matches)
      (setq start (match-end 0)))
    (nreverse matches)))

(defun prelude-test--modules ()
  "Return the names of all modules, without the \"prelude-\" prefix."
  (mapcar (lambda (file) (string-remove-prefix "prelude-" (file-name-base file)))
          (directory-files prelude-modules-dir nil "\\`prelude-.*\\.el\\'")))

(defun prelude-test--module-pages ()
  "Return the names of the module pages of the manual."
  (delete "index"
          (mapcar #'file-name-base
                  (directory-files (expand-file-name "docs/modules" prelude-dir)
                                   nil "\\.md\\'"))))

(ert-deftest prelude-sample-modules-file-lists-every-module ()
  (let ((listed (prelude-test--matches "(require 'prelude-\\([a-z-]+\\))"
                                       (prelude-test--file-string "sample/prelude-modules.el"))))
    ;; prelude-programming is pulled in by the language modules, and
    ;; prelude-coffee is a stub that only says the module is gone
    (should (equal (seq-difference (prelude-test--modules) listed)
                   '("coffee" "programming")))
    (should-not (seq-difference listed (prelude-test--modules)))))

(ert-deftest prelude-installation-docs-show-the-sample-modules-file ()
  (let ((sample (prelude-test--file-string "sample/prelude-modules.el"))
        (docs (prelude-test--file-string "docs/installation.md")))
    (should (string-match ";;; Uncomment the modules\\(?:.\\|\n\\)*?\n\n(provide" sample))
    (should (string-search (string-remove-suffix "\n\n(provide" (match-string 0 sample))
                           docs))))

(ert-deftest prelude-every-module-has-a-manual-page ()
  (let ((pages (prelude-test--module-pages)))
    (dolist (module (prelude-test--modules))
      (should (member (pcase module
                        ;; described on the page of another module
                        ("helm-everywhere" "helm")
                        ("lsp-mode" "lsp")
                        ("org" "orgmode")
                        ;; nothing to document
                        ("coffee" (car pages))
                        (_ module))
                      (append pages
                              (mapcar (lambda (page) (string-replace "_" "-" page))
                                      pages)))))))

(ert-deftest prelude-every-manual-page-is-linked ()
  "Module pages are in the site's navigation and in both module indexes."
  (let* ((pages (sort (prelude-test--module-pages) #'string<))
         (index (prelude-test--file-string "docs/modules/index.md"))
         (languages (substring index
                               (string-search "## Programming Language Modules" index)
                               (string-search "## Completion Frameworks" index))))
    (should (equal (sort (prelude-test--matches "modules/\\([a-z_-]+\\)\\.md"
                                                (prelude-test--file-string "mkdocs.yml"))
                         #'string<)
                   (sort (cons "index" (copy-sequence pages)) #'string<)))
    (should-not (seq-difference pages (prelude-test--matches "(\\([a-z_-]+\\)\\.md)" index)))
    (should (equal (prelude-test--matches "(modules/\\([a-z_-]+\\)\\.md)"
                                          (prelude-test--file-string "docs/index.md"))
                   (prelude-test--matches "(\\([a-z_-]+\\)\\.md)" languages)))))

;;; prelude-test.el ends here
