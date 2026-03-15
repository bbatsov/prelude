;;; prelude-ocaml.el --- Emacs Prelude: OCaml programming support.
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
;; Optionally install utop for a better REPL experience:
;;   opam install utop

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

(prelude-require-packages '(neocaml ocaml-eglot dune utop))

(with-eval-after-load 'neocaml
  (require 'ocaml-eglot)

  (defun prelude-ocaml-mode-defaults ()
    (subword-mode +1)
    (utop-minor-mode +1)
    (ocaml-eglot-setup))

  (setq prelude-ocaml-mode-hook 'prelude-ocaml-mode-defaults)

  (add-hook 'neocaml-mode-hook (lambda ()
                                 (run-hooks 'prelude-ocaml-mode-hook))))

(provide 'prelude-ocaml)

;;; prelude-ocaml.el ends here
