;;; prelude-scss.el --- Emacs Prelude: scss support
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for scss-mode.

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

(require 'prelude-css)

(defun prelude-scss-mode-defaults ()
  (prelude-css-mode-defaults))

;; SCSS (Sassy CSS) major mode
(use-package scss-mode
  :ensure t
  :custom
  ;; Don't auto-compile to CSS on save
  (scss-compile-at-save nil)
  :hook (scss-mode . (lambda () (run-hooks 'prelude-scss-mode-hook))))

(setq prelude-scss-mode-hook 'prelude-scss-mode-defaults)

(provide 'prelude-scss)
;;; prelude-scss.el ends here
