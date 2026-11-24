;;; prelude-apheleia.el --- Emacs Prelude: Async format-on-save via Apheleia.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Apheleia is a unified format-on-save framework for Emacs.  It runs
;; the appropriate code formatter (prettier, black, ruff, gofmt,
;; rustfmt, ...) asynchronously on save, then patches the buffer so
;; point/window state are preserved and there is no flicker.
;;
;; Enabling this module installs Apheleia and turns on
;; `apheleia-global-mode'.  Apheleia ships with sensible defaults
;; for many languages and tools out of the box; check
;; `apheleia-mode-alist' to see what gets formatted by what.
;;
;; A note on overlap: some Prelude language modules (Go, Rust) format
;; on save themselves.  They step aside in the buffers where Apheleia
;; is enabled (see `prelude-format-on-save-p').

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

(defvar apheleia-mode-alist)

(defun prelude-apheleia-prefer-goimports ()
  "Format Go code with goimports instead of gofmt if it's installed.
That's what `prelude-go' does when it formats the code itself."
  (when (executable-find "goimports")
    (dolist (mode '(go-mode go-ts-mode))
      (setf (alist-get mode apheleia-mode-alist) 'goimports))))

(use-package apheleia
  :ensure t
  :init
  (apheleia-global-mode +1)
  :config
  (prelude-apheleia-prefer-goimports))

(provide 'prelude-apheleia)
;;; prelude-apheleia.el ends here
