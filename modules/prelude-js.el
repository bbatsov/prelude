;;; prelude-js.el --- Emacs Prelude: JavaScript configuration.  -*- lexical-binding: t; -*-
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

(defun prelude-json-mode-defaults ()
  (subword-mode +1)
  ;; JSON is everywhere, a language server for it isn't
  (prelude-lsp-enable 'quiet))

(add-hook 'prelude-js-mode-hook #'prelude-js-mode-defaults)
(add-hook 'prelude-json-mode-hook #'prelude-json-mode-defaults)

;; `js-json-mode' is a child of `js-mode' in Emacs 29, but not in newer
;; versions, so JSON buffers are kept out of the JavaScript hook
(add-hook 'js-mode-hook (lambda ()
                          (unless (derived-mode-p 'js-json-mode)
                            (run-hooks 'prelude-js-mode-hook))))
(add-hook 'js-ts-mode-hook (lambda () (run-hooks 'prelude-js-mode-hook)))
(add-hook 'js-json-mode-hook (lambda () (run-hooks 'prelude-json-mode-hook)))
(add-hook 'json-ts-mode-hook (lambda () (run-hooks 'prelude-json-mode-hook)))

(provide 'prelude-js)

;;; prelude-js.el ends here
