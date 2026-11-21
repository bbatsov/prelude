;;; prelude-csharp.el --- Emacs Prelude: C# programming support.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Prelude configuration for C#, based on the major modes that come
;; with Emacs.

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

;; Use csharp-ts-mode when the tree-sitter grammar is available
(prelude-treesit-remap 'c-sharp 'csharp-mode 'csharp-ts-mode)

(defun prelude-csharp-mode-defaults ()
  "Default coding hook, useful with C#."
  ;; CamelCase aware editing operations
  (subword-mode +1)
  (prelude-lsp-enable))

(add-hook 'prelude-csharp-mode-hook #'prelude-csharp-mode-defaults)

(add-hook 'csharp-mode-hook (lambda ()
                              (run-hooks 'prelude-csharp-mode-hook)))
(add-hook 'csharp-ts-mode-hook (lambda ()
                                 (run-hooks 'prelude-csharp-mode-hook)))

(provide 'prelude-csharp)
;;; prelude-csharp.el ends here
