;;; prelude-haskell.el --- Emacs Prelude: Nice config for Haskell programming.
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Nice config for Haskell programming.  Install haskell-language-server
;; for LSP support:
;;   ghcup install hls

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

(defun prelude-haskell-mode-defaults ()
  (subword-mode +1)
  (eldoc-mode +1)
  ;; Haskell's layout-based indentation
  (haskell-indentation-mode +1)
  ;; GHCi integration: type info, jump to definition, REPL interaction
  (interactive-haskell-mode +1)
  (prelude-lsp-enable))

(use-package haskell-mode
  :ensure t
  :hook (haskell-mode . (lambda ()
                          (run-hooks 'prelude-haskell-mode-hook))))

(setq prelude-haskell-mode-hook 'prelude-haskell-mode-defaults)

(provide 'prelude-haskell)

;;; prelude-haskell.el ends here
