;;; prelude-fsharp.el --- Emacs Prelude: F# programming support.
;;
;; Author: Andre Boechat <andre.boechat@tutanota.com>

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

(require 'prelude-programming)

(defun prelude-fsharp-mode-defaults ()
  ;; Use the .NET CLI to run the F# interactive REPL
  (setq inferior-fsharp-program "dotnet fsi --readline-")
  (subword-mode +1)
  (when (eq prelude-lsp-client 'eglot)
    (require 'eglot-fsharp))
  (prelude-lsp-enable))

(use-package fsharp-mode
  :ensure t
  :hook (fsharp-mode . (lambda ()
                         (run-hooks 'prelude-fsharp-mode-hook))))

;; Auto-configures Eglot to use FsAutoComplete as the F# language
;; server.  Only needed when Eglot is the LSP client.
(use-package eglot-fsharp
  :ensure t
  :defer t
  :if (eq prelude-lsp-client 'eglot))

(setq prelude-fsharp-mode-hook 'prelude-fsharp-mode-defaults)

(provide 'prelude-fsharp)

;;; prelude-fsharp.el ends here
