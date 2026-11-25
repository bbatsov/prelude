;;; prelude-org.el --- Emacs Prelude: org-mode configuration.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for org-mode.

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

(defvar org-log-done)
(defvar org-log-into-drawer)
(declare-function org-indent-mode "org-indent")

;; a few useful global keybindings for org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(with-eval-after-load 'org
  (require 'org-habit)
  (setq org-log-done t)
  (setq org-log-into-drawer t))

(defun prelude-org-setup ()
  "Set up an Org buffer."
  ;; clean visual indentation
  (org-indent-mode +1)
  ;; let org-mode handle C-a
  (let ((oldmap (cdr (assoc 'prelude-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "C-a") 'org-beginning-of-line)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(prelude-mode . ,newmap) minor-mode-overriding-map-alist))
  (run-hooks 'prelude-org-mode-hook))

(make-obsolete-variable 'prelude-org-mode-hook
                        "use `org-mode-hook' instead."
                        "2.2.0")

(add-hook 'org-mode-hook #'prelude-org-setup -50)

(provide 'prelude-org)
;;; prelude-org.el ends here
