;;; prelude-julia.el --- Emacs Prelude: Julia programming support.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Prelude configuration for Julia: julia-mode, a REPL and LSP.

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

(declare-function eglot-jl-init "eglot-jl")

(defun prelude-julia-setup ()
  "Set up a Julia buffer."
  ;; CamelCase aware editing operations
  (subword-mode +1)
  ;; send code to a Julia REPL (C-c C-c, C-c C-z, etc.)
  (julia-repl-mode +1)
  ;; lsp-mode doesn't load this client on its own
  (when (eq prelude-lsp-client 'lsp-mode)
    (require 'lsp-julia nil t))
  (prelude-lsp-enable))

(use-package julia-mode
  :ensure t
  :defer t)

;; Run a Julia REPL in a terminal buffer and send code to it
(use-package julia-repl
  :ensure t
  :defer t)

;; Both LSP clients need a package that knows how to start (and
;; install) LanguageServer.jl.
(when (eq prelude-lsp-client 'eglot)
  (use-package eglot-jl
    :ensure t
    :defer t)

  ;; eglot-jl hooks into project.el, so it's only set up once Julia is
  ;; actually used.
  (with-eval-after-load 'julia-mode
    (with-eval-after-load 'eglot
      (when (fboundp 'eglot-jl-init)
        (eglot-jl-init)))))

(when (eq prelude-lsp-client 'lsp-mode)
  (use-package lsp-julia
    :ensure t
    :defer t))

(add-hook 'julia-mode-hook #'prelude-julia-setup -50)

(provide 'prelude-julia)
;;; prelude-julia.el ends here
