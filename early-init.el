;;; early-init.el --- Prelude's early configuration.
;;
;; Copyright (c) 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.1.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file is loaded before the package system and the UI are
;; initialized, which makes it the right place for a few settings that
;; have to be applied very early during startup.

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

;; Raise the garbage collection threshold as high as possible for the
;; duration of startup, so we don't pay for repeated collections while
;; a lot of code is being loaded.  A modest threshold is restored once
;; startup is over (a permanently huge threshold trades frequent short
;; pauses for rare, long freezes).
(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 50 1000 1000) ; 50MB
                  gc-cons-percentage 0.2)))

;; Disable the tool bar via frame parameters, so the initial frame is
;; never created with one in the first place.  Toggling `tool-bar-mode'
;; later forces an expensive frame resize.
(push '(tool-bar-lines . 0) default-frame-alist)

;; Don't resize the frame in response to font or UI changes during
;; startup - it's needless work before the frame is even visible.
(setq frame-inhibit-implied-resize t)

;; Native-compile packages when they are installed rather than lazily
;; on first load, so you don't hit compilation pauses while working.
(setq package-native-compile t)

;; GUI Emacs on macOS doesn't inherit the environment from the shell,
;; so without LANG it ends up in the "C" locale, which breaks things
;; like spell-checker dictionaries and subprocess sorting.
(when (and (eq system-type 'darwin) (not (getenv "LANG")))
  (setenv "LANG" "en_US.UTF-8"))

;;; early-init.el ends here
