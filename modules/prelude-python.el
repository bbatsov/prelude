;;; prelude-python.el --- Emacs Prelude: python.el configuration.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configuration for Python programming.  Uses python-ts-mode
;; (tree-sitter) when available and LSP for code navigation,
;; completion and diagnostics.

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

;; Use python-ts-mode when the tree-sitter grammar is available
(prelude-treesit-remap 'python 'python-mode 'python-ts-mode)

(when (and (fboundp 'exec-path-from-shell-copy-env)
           (prelude-fetch-shell-environment-p))
  (exec-path-from-shell-copy-env "PYTHONPATH"))

(declare-function python-imenu-create-flat-index "python")

(defun prelude-python-setup ()
  "Set up a Python buffer."
  (subword-mode +1)
  (eldoc-mode +1)
  (prelude-lsp-enable)
  (setq-local imenu-create-index-function
              #'python-imenu-create-flat-index)
  (run-hooks 'prelude-python-mode-hook))

(make-obsolete-variable 'prelude-python-mode-hook
                        "use `python-mode-hook' and `python-ts-mode-hook' instead."
                        "2.2.0")

(add-hook 'python-mode-hook #'prelude-python-setup -50)
(add-hook 'python-ts-mode-hook #'prelude-python-setup -50)

(provide 'prelude-python)
;;; prelude-python.el ends here
