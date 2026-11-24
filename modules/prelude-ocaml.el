;;; prelude-ocaml.el --- Emacs Prelude: OCaml programming support.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2014-2026 Geoff Shannon
;;
;; Author: Geoff Shannon <geoffpshannon@gmail.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A modern OCaml setup based on neocaml, eglot, and ocaml-eglot.

;; You'll need to install opam and ocaml-lsp-server:
;;   opam install ocaml-lsp-server

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

(defun prelude-ocaml-mode-defaults ()
  ;; CamelCase aware editing operations
  (subword-mode +1)
  ;; ocaml-eglot-mode adds OCaml-specific LSP commands
  ;; (switch .ml/.mli, type-enclosing, destruct, etc.).  Check that
  ;; it's actually available, as a failed install would otherwise
  ;; break the whole mode hook.
  (when (and (eq prelude-lsp-client 'eglot)
             (fboundp 'ocaml-eglot-mode))
    (ocaml-eglot-mode 1))
  ;; Start the LSP server (eglot-ensure or lsp-deferred)
  (prelude-lsp-enable))

;; Tree-sitter powered major mode for OCaml, Dune, and utop.
;; Replaces the older tuareg + merlin + utop stack.
;; Hook into neocaml-base-mode so both .ml and .mli buffers
;; get the same setup.
(use-package neocaml
  :ensure t
  ;; neocaml registers its modes with Eglot itself
  :hook (neocaml-base-mode . (lambda ()
                               (run-hooks 'prelude-ocaml-mode-hook))))

;; OCaml-specific Eglot extensions (requires ocaml-lsp-server).
;; Only installed when Eglot is the configured LSP client.
(when (eq prelude-lsp-client 'eglot)
  (use-package ocaml-eglot
    :ensure t
    :defer t))

(add-hook 'prelude-ocaml-mode-hook #'prelude-ocaml-mode-defaults)

(provide 'prelude-ocaml)
;;; prelude-ocaml.el ends here
