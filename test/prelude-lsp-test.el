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

(defun prelude-lsp-test--connect (module files)
  "Open the first of FILES in a new project, with MODULE loaded.
Return the program of the language server Eglot connected to, or nil
if it didn't connect in time."
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
                  (prelude-lsp-test--connected-server))
              (ignore-errors (eglot-shutdown-all))
              (set-buffer-modified-p nil)
              (kill-buffer))))
      (delete-directory dir t))))

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

(message "")
(if prelude-lsp-test-failures
    (progn
      (message "[test] FAILED - no language server for: %s"
               (mapconcat #'symbol-name (reverse prelude-lsp-test-failures) ", "))
      (kill-emacs 1))
  (message "[test] PASSED - connected to every language server that's installed.")
  (kill-emacs 0))

;;; prelude-lsp-test.el ends here
