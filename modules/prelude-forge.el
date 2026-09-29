;;; prelude-forge.el --- Emacs Prelude: Magit Forge configuration.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configuration for Forge, a Magit extension that lets you work with
;; pull requests, issues, and discussions on Git forges (GitHub,
;; GitLab, Gitea, etc.) directly from Emacs.
;;
;; Forge integrates with Magit automatically once loaded -- forge
;; sections appear in the Magit status buffer, and `'' (apostrophe)
;; opens the forge dispatch menu.  You'll need to configure an
;; authentication token; see the module documentation for details.

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

(use-package forge
  :ensure t
  :after magit)

(provide 'prelude-forge)
;;; prelude-forge.el ends here
