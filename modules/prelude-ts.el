;;; prelude-ts.el --- Emacs Prelude: TypeScript programming support.
;;
;; Copyright © 2023-2026 LEE Dongjun
;;
;; Author: LEE Dongjun <redongjun@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configuration for TypeScript development.  Uses
;; typescript-ts-mode (tree-sitter) when available and LSP for
;; code intelligence.

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

;; Use typescript-ts-mode when the tree-sitter grammar is available
(require 'treesit nil t)
(when (and (fboundp 'treesit-ready-p) (treesit-ready-p 'typescript t))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))

(defun prelude-ts-mode-defaults ()
  (subword-mode +1)
  (prelude-lsp-enable))

(setq prelude-ts-mode-hook 'prelude-ts-mode-defaults)

(add-hook 'typescript-ts-mode-hook (lambda ()
                                     (run-hooks 'prelude-ts-mode-hook)))
(add-hook 'tsx-ts-mode-hook (lambda ()
                              (run-hooks 'prelude-ts-mode-hook)))

(provide 'prelude-ts)

;;; prelude-ts.el ends here
