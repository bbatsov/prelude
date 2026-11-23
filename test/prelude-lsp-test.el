;;; prelude-lsp-test.el --- Connect to real language servers -*- lexical-binding: t; -*-

;;; Commentary:

;; Opens a file of a few languages in a throwaway project and waits for
;; Eglot to connect to the language server that Prelude's module asked
;; for.  That's the whole chain a user depends on: the module's hook,
;; `prelude-lsp-enable', Eglot's choice of a server and the server
;; itself.
;;
;; Languages whose server isn't installed are skipped, unless the
;; server is listed in the PRELUDE_LSP_TEST_SERVERS environment
;; variable (separated by spaces), in which case that's a failure.
;;
;; In the end a Go file with a mistake in it is opened, to see that what
;; the language server has to say about it arrives in Flycheck.
;;
;; Usage:
;;   emacs --batch -l init.el -l test/prelude-lsp-test.el

;;; Code:

(require 'cl-lib)
(require 'eglot)

(defvar prelude-lsp-test-timeout 60
  "How many seconds to wait for a language server to connect.")

(defvar prelude-lsp-test-cases
  '((prelude-c "clangd" ("main.c" . "int main(void) { return 0; }\n"))
    (prelude-go "gopls"
                ("main.go" . "package main\n\nfunc main() {}\n")
                ("go.mod" . "module example.com/prelude\n\ngo 1.21\n"))
    (prelude-python "pylsp" ("main.py" . "print(1)\n"))
    (prelude-shell "bash-language-server" ("run.sh" . "#!/bin/bash\necho hi\n"))
    (prelude-css "vscode-css-language-server" ("main.css" . "a { color: red; }\n"))
    (prelude-js "vscode-json-language-server" ("main.json" . "{\"a\": 1}\n"))
    (prelude-yaml "yaml-language-server" ("main.yml" . "a: 1\n")))
  "List of (MODULE SERVER FILE...), where a FILE is (NAME . CONTENTS).
The first file is the one that gets opened.")

(defvar prelude-lsp-test-required-servers
  (split-string (or (getenv "PRELUDE_LSP_TEST_SERVERS") ""))
  "Servers that have to be installed.")

(defvar prelude-lsp-test-failures nil)

(defun prelude-lsp-test--connected-server ()
  "Return the program of the server managing the current buffer, if any."
  (when (eglot-managed-p)
    (car (process-command (jsonrpc--process (eglot-current-server))))))

(defun prelude-lsp-test--connect (module files &optional then)
  "Open the first of FILES in a new project, with MODULE loaded.
Return the program of the language server Eglot connected to, or nil
if it didn't connect in time.  With THEN, return what that function
returns when called in the buffer after the server has connected; it's
given the time by which it has to be done."
  (require module)
  (let ((dir (make-temp-file "prelude-lsp-test" t))
        (prelude-treesit-auto-install nil))
    (unwind-protect
        (progn
          (pcase-dolist (`(,name . ,contents) files)
            (write-region contents nil (expand-file-name name dir) nil 'silent))
          (with-current-buffer (find-file-noselect (expand-file-name (caar files) dir))
            (unwind-protect
                (let ((deadline (+ (float-time) prelude-lsp-test-timeout)))
                  ;; Eglot connects after the first command in the buffer
                  (run-hooks 'post-command-hook)
                  (while (and (not (eglot-managed-p))
                              (< (float-time) deadline))
                    (accept-process-output nil 0.1))
                  (if (and then (eglot-managed-p))
                      (funcall then deadline)
                    (prelude-lsp-test--connected-server)))
              (ignore-errors (eglot-shutdown-all))
              (set-buffer-modified-p nil)
              (kill-buffer))))
      (delete-directory dir t))))

(defvar flycheck-current-errors)
(declare-function flycheck-mode "flycheck")
(declare-function flycheck-buffer "flycheck")
(declare-function flycheck-running-p "flycheck")
(declare-function flycheck-error-checker "flycheck")
(declare-function flycheck-error-message "flycheck")

(defun prelude-lsp-test--flycheck-errors (deadline)
  "Return the Flycheck errors of the current buffer as (CHECKER . MESSAGE).
Wait for them to show up until DEADLINE."
  ;; Flycheck stays out of the way in batch mode
  (flycheck-mode +1)
  ;; The server takes its time, and what it reports later makes Flycheck
  ;; check again only once Emacs is idle, which it never is in batch
  ;; mode.  So keep asking.
  (while (and (not flycheck-current-errors)
              (< (float-time) deadline))
    (unless (flycheck-running-p)
      (flycheck-buffer))
    (accept-process-output nil 1))
  (mapcar (lambda (err)
            (cons (flycheck-error-checker err) (flycheck-error-message err)))
          flycheck-current-errors))

(message "\n[test] Connecting to language servers...\n")

(pcase-dolist (`(,module ,server . ,files) prelude-lsp-test-cases)
  (cond
   ((executable-find server)
    (let ((connected (condition-case err
                         (prelude-lsp-test--connect module files)
                       (error (message "[test]     %s" (error-message-string err))
                              nil))))
      (if (and connected (string= (file-name-nondirectory connected) server))
          (message "[test]   ✓ %s (%s)" module server)
        (message "[test]   ✗ %s: expected %s, got %s" module server connected)
        (push module prelude-lsp-test-failures))))
   ((member server prelude-lsp-test-required-servers)
    (message "[test]   ✗ %s: %s isn't installed" module server)
    (push module prelude-lsp-test-failures))
   (t
    (message "[test]   - %s: %s isn't installed, skipped" module server))))

(message "\n[test] Looking for the diagnostics of a language server in Flycheck...\n")

(cond
 ((not (eq prelude-lsp-client 'eglot))
  (message "[test]   - not using Eglot, skipped"))
 ((executable-find "gopls")
  (let ((errors (condition-case err
                    (prelude-lsp-test--connect
                     'prelude-go
                     '(("main.go" . "package main\n\nfunc main() {\n\tunused := 1\n}\n")
                       ("go.mod" . "module example.com/prelude\n\ngo 1.21\n"))
                     #'prelude-lsp-test--flycheck-errors)
                  (error (message "[test]     %s" (error-message-string err))
                         nil))))
    (if (and (consp errors)
             (seq-some (lambda (err)
                         (and (eq (car err) 'eglot-check)
                              (string-match-p "declared and not used" (cdr err))))
                       errors))
        (message "[test]   ✓ gopls reported the unused variable")
      (message "[test]   ✗ expected an error about an unused variable from eglot-check, got %S"
               errors)
      (push 'flycheck prelude-lsp-test-failures))))
 ((member "gopls" prelude-lsp-test-required-servers)
  (message "[test]   ✗ gopls isn't installed")
  (push 'flycheck prelude-lsp-test-failures))
 (t
  (message "[test]   - gopls isn't installed, skipped")))

(message "")
(if prelude-lsp-test-failures
    (progn
      (message "[test] FAILED - problems with: %s"
               (mapconcat #'symbol-name (reverse prelude-lsp-test-failures) ", "))
      (kill-emacs 1))
  (message "[test] PASSED - every language server that's installed works.")
  (kill-emacs 0))

;;; prelude-lsp-test.el ends here
