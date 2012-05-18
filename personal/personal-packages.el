;;; personal-packages.el --- Emacs Prelude: Required package selection.
;;
;; Copyright (c) 2011-2012 Bozhidar Batsov
;; Additions by Abizer Nasir
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Takes care of installing the packages that I want on all my machiines


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
(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq url-http-attempt-keepalives nil)

(defvar personal-packages
  '(haskell-mode markdown-mode solarized-theme sass-mode textile-mode inf-ruby windresize)
  "A list of packages to ensure are installed at launch.")

(defun personal-packages-installed-p ()
  (loop for p in personal-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun personal-install-packages ()
  (unless (personal-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs Prelude is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (dolist (p personal-packages)
      (unless (package-installed-p p)
        (package-install p)))))

(personal-install-packages)

(provide 'personal-packages)
;;; personal-packages.el ends here
