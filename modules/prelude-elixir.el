;;; prelude-elixir.el --- Emacs Prelude: Elixir programming support.
;;
;; Copyright © 2014-2026 Samuel Tonini
;;
;; Author: Samuel Tonini <tonini.samuel@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configuration for Elixir development.  Uses elixir-ts-mode
;; (tree-sitter) when available and LSP for code intelligence.
;; You'll need ElixirLS or Lexical as language server.

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

;; Use elixir-ts-mode when the tree-sitter grammar is available
(if (treesit-ready-p 'elixir t)
    (progn
      (prelude-require-packages '(heex-ts-mode))
      (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-ts-mode))
      (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-ts-mode))
      (add-to-list 'auto-mode-alist '("mix\\.lock" . elixir-ts-mode)))
  (prelude-require-packages '(elixir-mode)))

(defun prelude-elixir-mode-defaults ()
  (subword-mode +1)
  (prelude-lsp-enable))

(setq prelude-elixir-mode-hook 'prelude-elixir-mode-defaults)

(add-hook 'elixir-mode-hook (lambda ()
                              (run-hooks 'prelude-elixir-mode-hook)))
(add-hook 'elixir-ts-mode-hook (lambda ()
                                 (run-hooks 'prelude-elixir-mode-hook)))

(provide 'prelude-elixir)

;;; prelude-elixir.el ends here
