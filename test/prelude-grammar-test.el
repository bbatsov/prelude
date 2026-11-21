;;; prelude-grammar-test.el --- Check Prelude's tree-sitter grammars -*- lexical-binding: t; -*-

;;; Commentary:

;; Installs the tree-sitter grammar of every language that Prelude's
;; modules set up, at the version Prelude would install for a user, and
;; checks that the font-lock queries of its major mode work with it.
;;
;; A tree-sitter mode is written against a particular version of its
;; grammar.  With a grammar that's too new (or too old) its queries
;; refer to nodes that don't exist, and the mode loses its highlighting.
;; Which version works differs between Emacs releases, so this has to
;; run on all of the supported ones.
;;
;; Building the grammars needs git and a C compiler.
;;
;; Usage:
;;   emacs --batch -l init.el -l test/prelude-grammar-test.el

;;; Code:

(require 'cl-lib)
(require 'treesit)

(defvar prelude-grammar-test-excluded-modules
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
  "Modules that aren't loaded, as none of them has to do with tree-sitter.")

(defvar prelude-grammar-test-failures nil
  "List of (LANGUAGE . PROBLEM) pairs.")

(defun prelude-grammar-test--setting-query (setting)
  "Return the query of the font-lock SETTING."
  (if (fboundp 'treesit-font-lock-setting-query)
      (treesit-font-lock-setting-query setting)
    (car setting)))

(defun prelude-grammar-test--setting-enabled-p (setting)
  "Return non-nil if the font-lock SETTING is in use.
Modes turn off the rules of the features they don't use."
  (if (fboundp 'treesit-font-lock-setting-enable)
      (treesit-font-lock-setting-enable setting)
    (nth 1 setting)))

(defun prelude-grammar-test--run-queries (lang)
  "Run the font-lock queries the current buffer has for LANG.
Return a cons of how many there are and the errors they signaled."
  (let ((count 0)
        (errors nil))
    (dolist (setting treesit-font-lock-settings)
      (let ((query (prelude-grammar-test--setting-query setting)))
        (when (and (prelude-grammar-test--setting-enabled-p setting)
                   (treesit-compiled-query-p query)
                   (eq (treesit-query-language query) lang))
          (setq count (1+ count))
          (condition-case err
              (treesit-query-capture
               (treesit-parser-root-node (treesit-parser-create lang))
               query)
            (error (push (error-message-string err) errors))))))
    (cons count (delete-dups errors))))

(defun prelude-grammar-test--check (lang ts-mode)
  "Check that TS-MODE works with the grammar Prelude installs for LANG.
Return a description of the problem, or nil if there's none."
  (cond
   ((not (fboundp ts-mode))
    (message "[test]   - %s: no %s in this Emacs, skipped" lang ts-mode)
    nil)
   ((not (seq-every-p (lambda (grammar)
                        (or (treesit-language-available-p grammar)
                            (prelude-treesit--install-grammar grammar ts-mode)))
                      (prelude-treesit--grammars lang ts-mode)))
    "couldn't install the grammar")
   (t
    (with-temp-buffer
      ;; `bash-ts-mode' switches to `sh-mode' unless it sees a Bash script
      (when (eq lang 'bash)
        (insert "#!/bin/bash\n"))
      ;; Some modes can use extra grammars (e.g. jsdoc in `js-ts-mode'),
      ;; and newer Emacsen offer to install those.  They're optional.
      (cl-letf (((symbol-function 'y-or-n-p) #'ignore)
                ((symbol-function 'yes-or-no-p) #'ignore))
        (let ((warning-minimum-level :emergency))
          (funcall ts-mode)))
      (cond
       ((not (treesit-parser-list))
        (format "%s didn't create a parser (ended up in %s)" ts-mode major-mode))
       (t
        (pcase-let ((`(,count . ,errors) (prelude-grammar-test--run-queries lang)))
          (cond
           ((zerop count)
            (format "%s has no font-lock queries for %s" ts-mode lang))
           (errors
            (format "%d of %d font-lock queries are broken, e.g. %s"
                    (length errors) count (car errors)))))))))))

(unless (treesit-available-p)
  (message "[test] FAILED - this Emacs was built without tree-sitter support")
  (kill-emacs 1))

(dolist (file (directory-files prelude-modules-dir nil "\\`prelude-.*\\.el\\'"))
  (let ((module (intern (file-name-base file))))
    (unless (memq module prelude-grammar-test-excluded-modules)
      (require module))))

(message "\n[test] Checking the tree-sitter grammars of %d languages...\n"
         (length prelude-treesit--languages))

(pcase-dolist (`(,lang . ,ts-mode) (reverse prelude-treesit--languages))
  (let ((problem (condition-case err
                     (prelude-grammar-test--check lang ts-mode)
                   (error (error-message-string err)))))
    (if problem
        (progn
          (message "[test]   ✗ %s (%s): %s" lang ts-mode problem)
          (push (cons lang problem) prelude-grammar-test-failures))
      (when (fboundp ts-mode)
        (message "[test]   ✓ %s (%s)" lang ts-mode)))))

(message "")
(if prelude-grammar-test-failures
    (progn
      (message "[test] FAILED - %d grammar(s) don't work with their mode"
               (length prelude-grammar-test-failures))
      (kill-emacs 1))
  (message "[test] PASSED - all grammars work with their modes.")
  (kill-emacs 0))

;;; prelude-grammar-test.el ends here
