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
(require 'prelude-lsp)

(prelude-require-packages '(go-mode
                            go-projectile
                            lsp-mode
                            lsp-ui
                            company
                            gotest))

(require 'go-projectile)

;; Ignore go test -c output files
(add-to-list 'completion-ignored-extensions ".test")

(define-key 'help-command (kbd "G") 'godoc)

;; Fix: super-save will cause go files to be saved when lsp-mode does
;; certain things, triggering lsp-format-buffer. This causes, inter alia,
;; commas to disappear when typing go function invocations
(add-to-list 'super-save-predicates
             (lambda () (not (eq major-mode 'go-mode))))

(with-eval-after-load 'go-mode
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

    ;; stop whitespace being highlighted
    (whitespace-toggle-options '(tabs))

    ;; CamelCase aware editing operations
    (subword-mode +1))

  ;; if yas is present, this enables yas-global-mode
  ;; which provides completion via company
  (if (fboundp 'yas-global-mode)
      (yas-global-mode))

  ;; configure lsp for go
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (add-hook 'go-mode-hook #'lsp-deferred)

  (setq prelude-go-mode-hook 'prelude-go-mode-defaults)
  (add-hook 'go-mode-hook (lambda ()
                            (run-hooks 'prelude-go-mode-hook))))

(provide 'prelude-go)
;;; prelude-go.el ends here
