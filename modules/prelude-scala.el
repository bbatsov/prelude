;;; prelude-scala.el --- Emacs Prelude: scala-mode configuration.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic support for the Scala programming language.
;; Install Metals for LSP support:
;;   https://scalameta.org/metals/docs/editors/emacs

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

(defun prelude-scala-setup ()
  "Set up a Scala buffer."
  (subword-mode +1)
  (prelude-lsp-enable)
  (run-hooks 'prelude-scala-mode-hook))

(make-obsolete-variable 'prelude-scala-mode-hook
                        "use `scala-mode-hook' instead."
                        "2.2.0")

(use-package scala-mode
  :ensure t
  :defer t)

(add-hook 'scala-mode-hook #'prelude-scala-setup -50)

(provide 'prelude-scala)
;;; prelude-scala.el ends here
