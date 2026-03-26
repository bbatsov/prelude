;;; prelude-yaml.el --- Emacs Prelude: YAML programming support.
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Prelude configuration for YAML.

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

;; Use yaml-ts-mode when the tree-sitter grammar is available,
;; otherwise fall back to yaml-mode from MELPA
(require 'treesit nil t)
(if (and (fboundp 'treesit-ready-p) (treesit-ready-p 'yaml t))
    (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
  (use-package yaml-mode
    :ensure t
    :defer t))

(defun prelude-yaml-mode-defaults ()
  (whitespace-mode +1)
  (subword-mode +1)
  (add-hook 'before-save-hook 'prelude-cleanup-maybe nil t))

(setq prelude-yaml-mode-hook 'prelude-yaml-mode-defaults)

(add-hook 'yaml-mode-hook (lambda ()
                            (run-hooks 'prelude-yaml-mode-hook)))
(add-hook 'yaml-ts-mode-hook (lambda ()
                               (run-hooks 'prelude-yaml-mode-hook)))

(provide 'prelude-yaml)
;;; prelude-yaml.el ends here
