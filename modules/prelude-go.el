;;; prelude-go.el --- Emacs Prelude: Go programming support.
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

;; Use go-ts-mode when the tree-sitter grammar is available
(prelude-treesit-remap 'go 'go-mode 'go-ts-mode)

;; Ignore go test -c output files
(add-to-list 'completion-ignored-extensions ".test")

;; Fix: super-save will cause go files to be saved when lsp-mode does
;; certain things, triggering lsp-format-buffer. This causes, inter alia,
;; commas to disappear when typing go function invocations
(add-to-list 'super-save-predicates
             (lambda () (not (eq major-mode 'go-mode))))

(defun prelude-go-mode-defaults ()
  ;; Add to default go-mode key bindings
  (let ((map go-mode-map))
    (define-key map (kbd "C-c a") 'go-test-current-project) ;; current package, really
    (define-key map (kbd "C-c m") 'go-test-current-file)
    (define-key map (kbd "C-c .") 'go-test-current-test)
    (define-key map (kbd "C-c b") 'go-run)
    (define-key map (kbd "C-h f") 'godoc-at-point))

  ;; Prefer goimports to gofmt if installed
  (let ((goimports (executable-find "goimports")))
    (when goimports
      (setq gofmt-command goimports)))

  ;; format before save
  (add-hook 'before-save-hook #'gofmt-before-save nil t)

  ;; Go uses tabs for indentation, so don't highlight them as problems
  (whitespace-toggle-options '(tabs))

  ;; CamelCase aware editing operations
  (subword-mode +1)

  (prelude-lsp-enable))

;; Major mode for Go; also provides gofmt integration.
;; go-ts-mode (built-in) is used when the tree-sitter grammar is
;; available, but go-mode is still needed for gofmt, godoc, etc.
(use-package go-mode
  :ensure t
  :defer t
  :bind (:map help-map ("G" . godoc))
  :hook ((go-mode . (lambda () (run-hooks 'prelude-go-mode-hook)))
         (go-ts-mode . (lambda () (run-hooks 'prelude-go-mode-hook)))))

;; Run Go tests from Emacs (C-c a/m/.)
(use-package gotest
  :ensure t
  :defer t)

(setq prelude-go-mode-hook 'prelude-go-mode-defaults)

(provide 'prelude-go)
;;; prelude-go.el ends here
