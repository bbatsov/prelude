;;; prelude-rust.el --- Emacs Prelude: Rust programming support.  -*- lexical-binding: t; -*-
;;
;; Authors: Doug MacEachern, Manoel Vilela, Ben Alex, Daniel Gerlach
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Prelude configuration for Rust

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

;; You may need to install the following packages on your system:
;; * rustc (Rust Compiler)
;; * cargo (Rust Package Manager)
;; * rustfmt (Rust Tool for formatting code)
;; * rust-analyzer as lsp server needs to be in global path, see:
;; https://rust-analyzer.github.io/manual.html#rust-analyzer-language-server-binary

;; Disable super-save in Rust buffers: auto-saving triggers
;; lsp-format-buffer, which causes severe hangs during autocomplete
(add-to-list 'super-save-predicates
             (lambda () (not (derived-mode-p 'rust-ts-mode 'rust-mode))))

;; Built-in tree-sitter mode for Rust (requires rust grammar)
;; (rust-mode is only used if it's installed, e.g. from older Preludes)
(prelude-treesit-auto-mode "\\.rs\\'" 'rust 'rust-ts-mode 'rust-mode)

;; Interface to Cargo commands (C-c C-c C-b to build, etc.)
(use-package cargo
  :ensure t
  :defer t)

(defun prelude-rust-setup ()
  "Set up a Rust buffer."
  (cargo-minor-mode +1)
  ;; format before save (rust-analyzer runs rustfmt)
  (add-hook 'before-save-hook #'prelude-lsp-format-on-save nil t)
  ;; CamelCase aware editing operations
  (subword-mode +1)
  (prelude-lsp-enable)
  (run-hooks 'prelude-rust-mode-hook))

(make-obsolete-variable 'prelude-rust-mode-hook
                        "use `rust-ts-mode-hook' (or `rust-mode-hook' for rust-mode) instead."
                        "2.2.0")

(defun prelude-rust-fallback-setup ()
  "Set up a `rust-mode' buffer, unless it's built on `rust-ts-mode'.
`rust-mode' can be, and then the setup has run from the hook of
`rust-ts-mode' already."
  (unless (derived-mode-p 'rust-ts-mode)
    (prelude-rust-setup)))

(add-hook 'rust-ts-mode-hook #'prelude-rust-setup -50)
(add-hook 'rust-mode-hook #'prelude-rust-fallback-setup -50)

;; Configures Flycheck to use cargo/clippy for Rust diagnostics
(use-package flycheck-rust
  :ensure t
  :hook (flycheck-mode . flycheck-rust-setup))

;; Major mode for Rusty Object Notation (.ron) files
(use-package ron-mode
  :ensure t
  :defer t)

(provide 'prelude-rust)
;;; prelude-rust.el ends here
