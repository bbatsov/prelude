;;; prelude-erlang.el --- Emacs Prelude: Erlang programming support.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Gleb Peregud
;;
;; Author: Gleb Peregud <gleber.p@gmail.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Erlang is a concurrent functional language.

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

(defvar erlang-compile-function)

(require 'prelude-programming)

(defun prelude-erlang-setup ()
  "Set up an Erlang buffer."
  (subword-mode +1)
  (prelude-lsp-enable)
  ;; Use Projectile's compile command instead of erlang-mode's default
  (when prelude-projectile
    (require 'projectile)
    (setq erlang-compile-function 'projectile-compile-project))
  (run-hooks 'prelude-erlang-mode-hook))

(make-obsolete-variable 'prelude-erlang-mode-hook
                        "use `erlang-mode-hook' instead."
                        "2.2.0")

;; Major mode for Erlang.  Install erlang_ls for LSP support:
;;   https://github.com/erlang-ls/erlang_ls
(use-package erlang
  :ensure t
  :defer t)

(add-hook 'erlang-mode-hook #'prelude-erlang-setup -50)

(provide 'prelude-erlang)
;;; prelude-erlang.el ends here
