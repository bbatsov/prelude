;;; prelude-windows.el --- Emacs Prelude: Windows-specific setup.
;;
;; Copyright © 2011-2023 Bozhidar Batsov
;;
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Additional setup that's useful when running Emacs in Windows.

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

;; Teach Emacs how to interpret various modifier keys
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super) ; Left Windows key

(setq w32-pass-rwindow-to-system nil)
(setq w32-rwindow-modifier 'super) ; Right Windows key

(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'hyper) ; Menu/App key

;; Git setup (assuming you've installed Git for Windows)
(when (file-exists-p "C:/Program Files/Git/bin")
  (add-to-list 'exec-path "C:/Program Files/Git/bin")
  (add-to-list 'exec-path "C:/Program Files/Git/mingw64/bin")
  (setenv "PATH" (concat "C:/Program Files/Git/bin;" "C:/Program Files/Git/mingw64/bin;" (getenv "PATH"))))

;; needed for arc-mode (it allows you to open archives in Emacs)
(if (file-exists-p "C:/Program Files/7-Zip")
    (add-to-list 'exec-path "C:/Program Files/7-Zip")
  (message "7-Zip not found. It's a good idea to install it."))

(provide 'prelude-windows)
;;; prelude-windows.el ends here
