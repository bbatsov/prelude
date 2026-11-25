;;; prelude-helm-everywhere.el --- Emacs Prelude: Helm everywhere.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2014-2026 Tu, Do Hoang
;;
;; Author: Tu, Do Hoang (tuhdo1710@gmail.com)
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Enable Helm everywhere with global key bindings to replace common
;; global bindings and `helm-mode' is activated to replace `completing-read'
;; with `helm-completing-read-default', so users can use Helm with every prompt.

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

(require 'prelude-helm)

(defvar eshell-mode-map)

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-m" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-h f" . helm-apropos)
         ("C-h r" . helm-info-emacs)
         ("C-h C-l" . helm-locate-library)
         :map prelude-mode-map
         ("C-c f" . helm-recentf)
         :map minibuffer-local-map
         ("C-c C-l" . helm-minibuffer-history)
         :map isearch-mode-map
         ("C-o" . helm-occur-from-isearch))
  :init
  (helm-mode 1))

(defun prelude-helm-eshell-setup ()
  "Use Helm to list the Eshell history."
  (substitute-key-definition 'eshell-list-history 'helm-eshell-history eshell-mode-map))

(add-hook 'eshell-mode-hook #'prelude-helm-eshell-setup)

(when prelude-projectile
  ;; enable Helm version of Projectile with replacment commands
  (if (fboundp 'helm-projectile-mode) ; helm-projectile 1.7.0+
      (helm-projectile-mode +1)
    (with-suppressed-warnings ((obsolete helm-projectile-on))
      (helm-projectile-on))))

(provide 'prelude-helm-everywhere)
;;; prelude-helm-everywhere.el ends here
