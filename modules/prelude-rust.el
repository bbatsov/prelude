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


(prelude-require-packages '(rust-mode
                            cargo
                            flycheck-rust
                            tree-sitter
                            tree-sitter-langs
                            yasnippet
                            ron-mode))

(require 'tree-sitter)
(require 'tree-sitter-langs)

(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)

  ;; enable lsp for rust, by default it uses rust-analyzer as lsp server
  (add-hook 'rust-mode-hook 'lsp)

  ;; enable tree-sitter for nicer syntax highlighting
  (add-hook 'rust-mode-hook #'tree-sitter-mode)
  (add-hook 'rust-mode-hook #'tree-sitter-hl-mode)

  (defun prelude-rust-mode-defaults ()
    ;; format on save
    (setq rust-format-on-save t)

    ;; lsp settings
    (setq
     ;; enable macro expansion
     lsp-rust-analyzer-proc-macro-enable t
     lsp-rust-analyzer-experimental-proc-attr-macros t)

    ;; Prevent #! from chmodding rust files to be executable
    (remove-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

    ;; snippets are required for correct lsp autocompletions
    (yas-minor-mode)

    ;; CamelCase aware editing operations
    (subword-mode +1))

  (setq prelude-rust-mode-hook 'prelude-rust-mode-defaults)

  (add-hook 'rust-mode-hook (lambda ()
                              (run-hooks 'prelude-rust-mode-hook))))

(provide 'prelude-rust)
;;; prelude-rust.el ends here
