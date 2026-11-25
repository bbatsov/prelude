;;; prelude-nix.el --- Emacs Prelude: Nix programming support.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Prelude configuration for Nix: nix-mode and LSP.

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

;; You may need to install a language server on your system:
;; nil or nixd.

(defun prelude-nix-setup ()
  "Set up a Nix buffer."
  (prelude-lsp-enable))

(use-package nix-mode
  :ensure t
  :defer t)

(add-hook 'nix-mode-hook #'prelude-nix-setup -50)

(provide 'prelude-nix)
;;; prelude-nix.el ends here
