;;; prelude-r.el --- Emacs Prelude: R programming support.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Prelude configuration for R, based on ESS (Emacs Speaks Statistics).

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

;; You may need to install the language server (it's an R package):
;;   install.packages("languageserver")

(defun prelude-r-mode-defaults ()
  "Default coding hook, useful with R."
  (prelude-lsp-enable))

(use-package ess
  :ensure t
  :defer t
  :hook (ess-r-mode . (lambda () (run-hooks 'prelude-r-mode-hook))))

(add-hook 'prelude-r-mode-hook #'prelude-r-mode-defaults)

(provide 'prelude-r)
;;; prelude-r.el ends here
