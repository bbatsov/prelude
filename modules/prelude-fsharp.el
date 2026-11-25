;;; prelude-fsharp.el --- Emacs Prelude: F# programming support.  -*- lexical-binding: t; -*-
;;
;; Author: Andre Boechat <andre.boechat@tutanota.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Basic setup for F# programming based on fsharp-mode and Eglot.

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

(defvar inferior-fsharp-program)

(require 'prelude-programming)

(defun prelude-fsharp-setup ()
  "Set up an F# buffer."
  ;; Use the .NET CLI to run the F# interactive REPL
  (setq inferior-fsharp-program "dotnet fsi --readline-")
  (subword-mode +1)
  ;; a failed install shouldn't break the whole mode hook
  (when (eq prelude-lsp-client 'eglot)
    (require 'eglot-fsharp nil t))
  (prelude-lsp-enable)
  (run-hooks 'prelude-fsharp-mode-hook))

(make-obsolete-variable 'prelude-fsharp-mode-hook
                        "use `fsharp-mode-hook' instead."
                        "2.2.0")

(use-package fsharp-mode
  :ensure t
  :defer t)

;; Auto-configures Eglot to use FsAutoComplete as the F# language
;; server.  Only needed when Eglot is the LSP client.
(when (eq prelude-lsp-client 'eglot)
  (use-package eglot-fsharp
    :ensure t
    :defer t))

(add-hook 'fsharp-mode-hook #'prelude-fsharp-setup -50)

(provide 'prelude-fsharp)
;;; prelude-fsharp.el ends here
