;;; prelude-rust.el --- Emacs Prelude: Rust programming support.
;;
;; Authors: Doug MacEachern, Manoel Vilela
;; Version: 1.0.1
;; Keywords: convenience rust

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

;; You may need installing the following packages on your system:
;; * rustc (Rust Compiler)
;; * cargo (Rust Package Manager)
;; * racer (Rust Completion Tool)
;; * rustfmt (Rust Tool for formatting code)

(prelude-require-packages '(rust-mode
                            racer
                            flycheck-rust
                            cargo))

(setq rust-format-on-save t)

(eval-after-load 'rust-mode
  '(progn
     (add-hook 'rust-mode-hook 'racer-mode)
     (add-hook 'racer-mode-hook 'eldoc-mode)
     (add-hook 'rust-mode-hook 'cargo-minor-mode)
     (add-hook 'rust-mode-hook 'flycheck-rust-setup)
     (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)

     (defun prelude-rust-mode-defaults ()
       (local-set-key (kbd "C-c C-d") 'racer-describe)
       ;; CamelCase aware editing operations
       (subword-mode +1))

     (setq prelude-rust-mode-hook 'prelude-rust-mode-defaults)

     (add-hook 'rust-mode-hook (lambda ()
                               (run-hooks 'prelude-rust-mode-hook)))))

(provide 'prelude-rust)
;;; prelude-rust.el ends here
