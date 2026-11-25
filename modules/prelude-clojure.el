;;; prelude-clojure.el --- Emacs Prelude: Clojure programming configuration.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A basic setup for Clojure programming based on clojure-mode
;; and CIDER.

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

(require 'prelude-lisp)

(defun prelude-clojure-setup ()
  "Set up a Clojure buffer."
  (subword-mode +1)
  (prelude-lisp-coding-setup)
  (run-hooks 'prelude-clojure-mode-hook))

(make-obsolete-variable 'prelude-clojure-mode-hook
                        "use `clojure-mode-hook' and `clojure-ts-mode-hook' instead."
                        "2.2.0")

(defun prelude-cider-repl-setup ()
  "Set up a CIDER REPL buffer."
  (subword-mode +1)
  (prelude-interactive-lisp-coding-setup)
  (run-hooks 'prelude-cider-repl-mode-hook))

(make-obsolete-variable 'prelude-cider-repl-mode-hook
                        "use `cider-repl-mode-hook' instead."
                        "2.2.0")

;; For tree-sitter support on Emacs 30+, install clojure-ts-mode
;; separately; it remaps clojure-mode buffers when it's present.
(use-package clojure-mode
  :ensure t
  :defer t)

;; CIDER: Clojure Interactive Development Environment that Rocks.
;; (To debug it, set `nrepl-log-messages' to log all nREPL messages.)
(use-package cider
  :ensure t
  :defer t)

(add-hook 'clojure-mode-hook #'prelude-clojure-setup -50)
;; clojure-ts-mode isn't built on clojure-mode
(add-hook 'clojure-ts-mode-hook #'prelude-clojure-setup -50)
(add-hook 'cider-repl-mode-hook #'prelude-cider-repl-setup -50)

(provide 'prelude-clojure)
;;; prelude-clojure.el ends here
