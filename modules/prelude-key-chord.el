;;; prelude-key-chord.el --- Key chord setup
;;
;; Copyright © 2011-2018 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configure key-chord key bindings.

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
(prelude-require-package 'key-chord)

(require 'key-chord)

(key-chord-define-global "jj" 'avy-goto-word-1)
(key-chord-define-global "jl" 'avy-goto-line)
(key-chord-define-global "jk" 'avy-goto-char)
(key-chord-define-global "JJ" 'crux-switch-to-previous-buffer)
(key-chord-define-global "uu" 'undo-tree-visualize)
(key-chord-define-global "xx" 'execute-extended-command)
(key-chord-define-global "yy" 'browse-kill-ring)

(defvar key-chord-tips '("Press <jj> quickly to jump to the beginning of a visible word."
                         "Press <jl> quickly to jump to a visible line."
                         "Press <jk> quickly to jump to a visible character."
                         "Press <JJ> quickly to switch to previous buffer."
                         "Press <uu> quickly to visualize the undo tree."
                         "Press <xx> quickly to execute extended command."
                         "Press <yy> quickly to browse the kill ring."))

(setq prelude-tips (append prelude-tips key-chord-tips))

(key-chord-mode +1)

(provide 'prelude-key-chord)

;;; prelude-key-chord.el ends here
