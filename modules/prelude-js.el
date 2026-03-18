;;; prelude-js.el --- Emacs Prelude: JavaScript configuration.
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configuration for JavaScript development.  Uses js-ts-mode
;; (tree-sitter) when available and LSP for code intelligence.

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

;; Use tree-sitter modes when grammars are available
(prelude-treesit-remap 'javascript 'js-mode 'js-ts-mode)
(prelude-treesit-remap 'json 'js-json-mode 'json-ts-mode)

(defun prelude-js-mode-defaults ()
  (subword-mode +1)
  (prelude-lsp-enable))

(setq prelude-js-mode-hook 'prelude-js-mode-defaults)

(add-hook 'js-mode-hook (lambda () (run-hooks 'prelude-js-mode-hook)))
(add-hook 'js-ts-mode-hook (lambda () (run-hooks 'prelude-js-mode-hook)))

(provide 'prelude-js)

;;; prelude-js.el ends here
