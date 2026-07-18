;;; prelude-swift.el --- Emacs Prelude: Swift programming support.
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Prelude configuration for Swift, based on the tree-sitter powered
;; `swift-ts-mode' and the sourcekit-lsp language server (which ships
;; with the Xcode command line tools or a Swift toolchain).

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
;; * a Swift toolchain (swift, swiftc)
;; * sourcekit-lsp (bundled with the Xcode command line tools)

;; Eglot has no built-in entry for Swift, so point it at sourcekit-lsp.
;; (lsp-mode users get Swift support via the lsp-sourcekit package.)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(swift-ts-mode . ("sourcekit-lsp"))))

(defun prelude-swift-mode-defaults ()
  "Default coding hook, useful with Swift."
  ;; CamelCase aware editing operations
  (subword-mode +1)
  (prelude-lsp-enable))

(setq prelude-swift-mode-hook 'prelude-swift-mode-defaults)

;; Tree-sitter based major mode for Swift (requires the swift grammar)
(use-package swift-ts-mode
  :ensure t
  :mode "\\.swift\\'"
  :hook (swift-ts-mode . (lambda () (run-hooks 'prelude-swift-mode-hook))))

(provide 'prelude-swift)
;;; prelude-swift.el ends here
