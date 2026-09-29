;;; prelude-eglot-booster.el --- Emacs Prelude: Eglot performance booster.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Speeds up Eglot by routing its JSON-RPC traffic through
;; `emacs-lsp-booster' -- a small Rust wrapper that pre-parses LSP
;; messages before handing them to Emacs.  The win is most visible
;; with verbose servers (rust-analyzer, gopls, tsserver) on large
;; projects.
;;
;; The module pulls in eglot-booster from GitHub via
;; `package-vc-install' and enables `eglot-booster-mode' once Eglot
;; loads -- but only if the `emacs-lsp-booster' binary is on your
;; PATH.  If it isn't, the module logs a hint and exits cleanly so
;; Eglot continues to work as before.
;;
;; The booster binary is a separate install -- see the module
;; documentation for details.

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

(declare-function eglot-booster-mode "eglot-booster")

(defvar prelude-eglot-booster-binary "emacs-lsp-booster"
  "Name of the lsp-booster binary, looked up via `executable-find'.")

(defvar prelude-eglot-booster-repo
  "https://github.com/jdtsmith/eglot-booster"
  "Source repository for the eglot-booster Emacs package.")

(if (executable-find prelude-eglot-booster-binary)
    (progn
      (unless (package-installed-p 'eglot-booster)
        (package-vc-install prelude-eglot-booster-repo))
      (use-package eglot-booster
        :after eglot
        :config (eglot-booster-mode)))
  (message "[prelude-eglot-booster] %s not found in PATH; \
install it from https://github.com/blahgeek/emacs-lsp-booster/releases \
to enable LSP acceleration."
           prelude-eglot-booster-binary))

(provide 'prelude-eglot-booster)
;;; prelude-eglot-booster.el ends here
