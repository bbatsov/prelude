;;; prelude-scss.el --- Emacs Prelude: scss support  -*- lexical-binding: t; -*-
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

;; Emacs has a `scss-mode' of its own (it lives in css-mode.el).  It's a
;; child of `css-mode', so it gets all of the CSS module's defaults.
;;
;; Prelude used to install the scss-mode package from MELPA, which was
;; last updated in 2018 and fails to load in a modern Emacs, leaving
;; SCSS files in `fundamental-mode'.  Make sure the built-in mode wins
;; for those who still have that package around.
(autoload 'scss-mode "css-mode" nil t)

;; SCSS buffers need nothing on top of that, but the obsolete hook
;; still runs, for the configs that use it
(defun prelude-scss-setup ()
  "Set up an SCSS buffer."
  (run-hooks 'prelude-scss-mode-hook))

(make-obsolete-variable 'prelude-scss-mode-hook
                        "use `scss-mode-hook' instead."
                        "2.2.0")

(add-hook 'scss-mode-hook #'prelude-scss-setup -50)

(provide 'prelude-scss)
;;; prelude-scss.el ends here
