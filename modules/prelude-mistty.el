;;; prelude-mistty.el --- Emacs Prelude: mistty terminal.
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; MisTTY is a shell/comint hybrid built on top of term.el: it gives
;; you full terminal emulation for TUI programs while keeping ordinary
;; Emacs editing and motion on the command line.  Unlike vterm it's
;; pure Emacs Lisp, so there's no native module to compile.

;; This module rebinds Prelude's terminal key (`C-c t') from
;; `crux-visit-term-buffer' to `mistty', on the assumption that if
;; you've enabled it you'd rather that key opened MisTTY.

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

(require 'prelude-mode)

(use-package mistty
  :ensure t
  :commands (mistty)
  :init
  ;; take over Prelude's terminal key inside prelude-mode-map (a global
  ;; binding would be shadowed by that minor-mode map)
  (define-key prelude-mode-map (kbd "C-c t") #'mistty))

(provide 'prelude-mistty)
;;; prelude-mistty.el ends here
