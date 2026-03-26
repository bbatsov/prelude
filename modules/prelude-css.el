;;; prelude-css.el --- Emacs Prelude: css support
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for css-mode.

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

;; Use css-ts-mode when the tree-sitter grammar is available
(prelude-treesit-remap 'css 'css-mode 'css-ts-mode)

(setq css-indent-offset 2)

;; Colorize color names and hex values in CSS buffers
(use-package rainbow-mode
  :ensure t
  :defer t)

(defun prelude-css-mode-defaults ()
  (rainbow-mode +1)
  (run-hooks 'prelude-prog-mode-hook))

(setq prelude-css-mode-hook 'prelude-css-mode-defaults)

(add-hook 'css-mode-hook (lambda ()
                           (run-hooks 'prelude-css-mode-hook)))
(add-hook 'css-ts-mode-hook (lambda ()
                              (run-hooks 'prelude-css-mode-hook)))

(provide 'prelude-css)
;;; prelude-css.el ends here
