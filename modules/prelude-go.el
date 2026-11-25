;;; prelude-go.el --- Emacs Prelude: Go programming support.  -*- lexical-binding: t; -*-
;;
;; Author: Doug MacEachern
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Prelude configuration for Go

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'prelude-programming)

(defvar go-ts-mode-map)

;; go-mode doesn't autoload these, and go-ts-mode buffers need them too
(autoload 'gofmt "go-mode" nil t)
(autoload 'godoc-at-point "go-mode" nil t)

;; Use go-ts-mode when the tree-sitter grammar is available
(prelude-treesit-remap 'go 'go-mode 'go-ts-mode)

;; Ignore go test -c output files
(add-to-list 'completion-ignored-extensions ".test")

;; Fix: super-save will cause go files to be saved when lsp-mode does
;; certain things, triggering lsp-format-buffer. This causes, inter alia,
;; commas to disappear when typing go function invocations
(add-to-list 'super-save-predicates
             (lambda () (not (derived-mode-p 'go-mode 'go-ts-mode))))

(defun prelude-go--define-keys (map)
  "Add Prelude's Go key bindings to MAP.
The test keys are the ones that `go-ts-mode' has had since Emacs 31."
  (define-key map (kbd "C-c C-t t") #'go-test-current-test)
  (define-key map (kbd "C-c C-t f") #'go-test-current-file)
  ;; the current package, really
  (define-key map (kbd "C-c C-t p") #'go-test-current-project)
  (define-key map (kbd "C-c C-r") #'go-run))

;; go-ts-mode has a keymap of its own
(with-eval-after-load 'go-mode
  (prelude-go--define-keys go-mode-map))
(with-eval-after-load 'go-ts-mode
  (prelude-go--define-keys go-ts-mode-map))

(defun prelude-go-format-buffer ()
  "Format the buffer with gofmt (or goimports), see `prelude-format-on-save-p'.
Unlike `gofmt-before-save', this works in `go-ts-mode' as well."
  (when (prelude-format-on-save-p)
    (gofmt)))

(defun prelude-go-setup ()
  "Set up a Go buffer."
  ;; Prefer goimports to gofmt if installed
  (let ((goimports (executable-find "goimports")))
    (when goimports
      (setq gofmt-command goimports)))

  ;; format before save
  (add-hook 'before-save-hook #'prelude-go-format-buffer nil t)

  ;; Go uses tabs for indentation, so don't highlight them as problems
  (whitespace-toggle-options '(tabs))

  ;; CamelCase aware editing operations
  (subword-mode +1)

  (prelude-lsp-enable)
  (run-hooks 'prelude-go-mode-hook))

(make-obsolete-variable 'prelude-go-mode-hook
                        "use `go-mode-hook' and `go-ts-mode-hook' instead."
                        "2.2.0")

;; Major mode for Go; also provides gofmt integration.
;; go-ts-mode (built-in) is used when the tree-sitter grammar is
;; available, but go-mode is still needed for gofmt, godoc, etc.
(use-package go-mode
  :ensure t
  :defer t
  :bind (:map help-map ("G" . godoc)))

;; Run Go tests from Emacs (C-c C-t t/f/p)
(use-package gotest
  :ensure t
  :defer t)

(add-hook 'go-mode-hook #'prelude-go-setup -50)
(add-hook 'go-ts-mode-hook #'prelude-go-setup -50)

(provide 'prelude-go)
;;; prelude-go.el ends here
