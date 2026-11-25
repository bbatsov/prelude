;;; prelude-terraform.el --- Emacs Prelude: Terraform programming support.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Prelude configuration for Terraform: terraform-mode and LSP.

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

;; You may need to install the following on your system:
;; * terraform (or tofu)
;; * terraform-ls

(defun prelude-terraform-setup ()
  "Set up a Terraform buffer."
  (prelude-lsp-enable))

(use-package terraform-mode
  :ensure t
  :defer t)

(add-hook 'terraform-mode-hook #'prelude-terraform-setup -50)

(provide 'prelude-terraform)
;;; prelude-terraform.el ends here
