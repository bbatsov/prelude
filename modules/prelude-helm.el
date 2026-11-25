;;; prelude-helm.el --- Emacs Prelude: Helm setup.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some configuration for Helm following this guide:
;; http://tuhdo.github.io/helm-intro.html

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

;; Helm gets loaded right away, so that its commands are under C-c h
;; from the start (the keymap isn't autoloaded).
(use-package helm
  :ensure t
  :demand t
  :custom
  ;; See https://github.com/bbatsov/prelude/pull/670 for a detailed
  ;; discussion of these options.
  (helm-split-window-inside-p t)
  (helm-buffers-fuzzy-matching t)
  (helm-move-to-line-cycle-in-source t)
  (helm-ff-search-library-in-sexp t)
  :bind (:map helm-command-map
              ("o" . helm-occur)
              ("g" . helm-do-grep-ag)
              ("SPC" . helm-all-mark-rings))
  :init
  (when (executable-find "curl")
    (setq helm-net-prefer-curl t))
  :config
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Helm moves the binding when the option is set with `setopt' (or
  ;; Customize), and this leaves alone a prefix of your own, set in
  ;; personal/preload.
  (when (equal helm-command-prefix-key
               (eval (car (get 'helm-command-prefix-key 'standard-value)) t))
    (setopt helm-command-prefix-key "C-c h")))

(when prelude-projectile
  (use-package helm-projectile
    :ensure t
    :demand t))

(push "Press <C-c p h> to navigate a project in Helm." prelude-tips)

(provide 'prelude-helm)
;;; prelude-helm.el ends here
