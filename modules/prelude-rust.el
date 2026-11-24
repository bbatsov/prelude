;;; prelude-rust.el --- Emacs Prelude: Rust programming support.  -*- lexical-binding: t; -*-
;;
;; Authors: Doug MacEachern, Manoel Vilela, Ben Alex, Daniel Gerlach

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

;; Disable super-save in Rust buffers -- auto-saving triggers
;; lsp-format-buffer which causes severe hangs during autocomplete
(add-to-list 'super-save-predicates
             (lambda () (not (derived-mode-p 'rust-ts-mode 'rust-mode))))

(defun prelude-rust-mode-defaults ()
  ;; format before save (rust-analyzer runs rustfmt)
  (add-hook 'before-save-hook #'prelude-lsp-format-on-save nil t)

  ;; CamelCase aware editing operations
  (subword-mode +1)

  (prelude-lsp-enable))

;; Built-in tree-sitter mode for Rust (requires rust grammar)
;; (rust-mode is only used if it's installed, e.g. from older Preludes)
(prelude-treesit-auto-mode "\\.rs\\'" 'rust 'rust-ts-mode 'rust-mode)

;; Interface to Cargo commands (C-c C-c C-b to build, etc.)
(use-package cargo
  :ensure t
  :defer t)

(defun prelude-rust-setup ()
  "Set up a Rust buffer: turn on Cargo, run `prelude-rust-mode-hook'.
The hook comes last, so that the functions on it can change anything."
  (cargo-minor-mode +1)
  (run-hooks 'prelude-rust-mode-hook))

(add-hook 'rust-ts-mode-hook #'prelude-rust-setup)
;; the fallback, unless it's built on rust-ts-mode (which rust-mode can
;; be), in which case the hook above has done the job
(add-hook 'rust-mode-hook (lambda ()
                            (unless (derived-mode-p 'rust-ts-mode)
                              (prelude-rust-setup))))

;; Configures Flycheck to use cargo/clippy for Rust diagnostics
(use-package flycheck-rust
  :ensure t
  :hook (flycheck-mode . flycheck-rust-setup))

;; Major mode for Rusty Object Notation (.ron) files
(use-package ron-mode
  :ensure t
  :defer t)

(add-hook 'prelude-rust-mode-hook #'prelude-rust-mode-defaults)

(provide 'prelude-rust)
;;; prelude-rust.el ends here
