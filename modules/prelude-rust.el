;;; prelude-rust.el --- Emacs Prelude: Rust programming support.
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
             (lambda () (not (eq major-mode 'rust-ts-mode))))

(defun prelude-rust-mode-defaults ()
  (setq rust-format-on-save t)

  ;; Rust files start with #![...] (inner attributes), which looks
  ;; like a shebang to Emacs -- prevent auto-chmod on save
  (remove-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

  ;; CamelCase aware editing operations
  (subword-mode +1)

  (prelude-lsp-enable))

;; Built-in tree-sitter mode for Rust (requires rust grammar)
(use-package rust-ts-mode
  :ensure t
  :hook (rust-ts-mode . (lambda () (run-hooks 'prelude-rust-mode-hook))))

;; Interface to Cargo commands (C-c C-c C-b to build, etc.)
(use-package cargo
  :ensure t
  :hook (rust-ts-mode . cargo-minor-mode))

;; Configures Flycheck to use cargo/clippy for Rust diagnostics
(use-package flycheck-rust
  :ensure t
  :hook (flycheck-mode . flycheck-rust-setup))

;; Major mode for Rusty Object Notation (.ron) files
(use-package ron-mode
  :ensure t
  :defer t)

(setq prelude-rust-mode-hook 'prelude-rust-mode-defaults)

(provide 'prelude-rust)
;;; prelude-rust.el ends here
