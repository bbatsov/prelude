;;; prelude-mode.el --- Emacs Prelude: minor mode
;;
;; Copyright © 2011-2018 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A minor mode defining a local keymap, plus a menu.

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
(require 'easymenu)
(require 'imenu-anywhere)
(require 'crux)

(defvar prelude-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c o") 'crux-open-with)
    (define-key map (kbd "C-c g") 'prelude-google)
    (define-key map (kbd "C-c G") 'prelude-github)
    (define-key map (kbd "C-c y") 'prelude-youtube)
    (define-key map (kbd "C-c U") 'prelude-duckduckgo)
    ;; mimic popular IDEs binding, note that it doesn't work in a terminal session
    (define-key map (kbd "C-a") 'crux-move-beginning-of-line)
    (define-key map [(shift return)] 'crux-smart-open-line)
    (define-key map (kbd "M-o") 'crux-smart-open-line)
    (define-key map [(control shift return)] 'crux-smart-open-line-above)
    (define-key map [(control shift up)]  'move-text-up)
    (define-key map [(control shift down)]  'move-text-down)
    (define-key map [(meta shift up)]  'move-text-up)
    (define-key map [(meta shift down)]  'move-text-down)
    (define-key map (kbd "C-c n") 'crux-cleanup-buffer-or-region)
    (define-key map (kbd "C-c f")  'crux-recentf-find-file)
    (define-key map (kbd "C-M-z") 'crux-indent-defun)
    (define-key map (kbd "C-c u") 'crux-view-url)
    (define-key map (kbd "C-c e") 'crux-eval-and-replace)
    (define-key map (kbd "C-c s") 'crux-swap-windows)
    (define-key map (kbd "C-c D") 'crux-delete-file-and-buffer)
    (define-key map (kbd "C-c d") 'crux-duplicate-current-line-or-region)
    (define-key map (kbd "C-c M-d") 'crux-duplicate-and-comment-current-line-or-region)
    (define-key map (kbd "C-c r") 'crux-rename-buffer-and-file)
    (define-key map (kbd "C-c t") 'crux-visit-term-buffer)
    (define-key map (kbd "C-c k") 'crux-kill-other-buffers)
    (define-key map (kbd "C-c TAB") 'crux-indent-rigidly-and-copy-to-clipboard)
    (define-key map (kbd "C-c I") 'crux-find-user-init-file)
    (define-key map (kbd "C-c S") 'crux-find-shell-init-file)
    (define-key map (kbd "C-c i") 'imenu-anywhere)
    ;; extra prefix for projectile
    (define-key map (kbd "s-p") 'projectile-command-map)
    (define-key map (kbd "C-c p") 'projectile-command-map)
    ;; make some use of the Super key
    (define-key map (kbd "s-r") 'crux-recentf-find-file)
    (define-key map (kbd "s-j") 'crux-top-join-line)
    (define-key map (kbd "s-k") 'crux-kill-whole-line)
    (define-key map (kbd "s-m m") 'magit-status)
    (define-key map (kbd "s-m l") 'magit-log)
    (define-key map (kbd "s-m f") 'magit-log-buffer-file)
    (define-key map (kbd "s-m b") 'magit-blame)
    (define-key map (kbd "s-o") 'crux-smart-open-line-above)
    (easy-menu-define prelude-mode-menu map
      "Prelude's menu."
      '("Prelude"
        ("Files"
         ["Open with..." crux-open-with]
         ["Re-open as root" crux-reopen-as-root]
         ["Delete file and buffer" crux-delete-file-and-buffer]
         ["Rename buffer and file" crux-rename-buffer-and-file]
         ["Find init file" crux-find-user-init-file]
         ["Find custom file" crux-find-user-custom-file]
         ["Find shell config file" crux-find-shell-init-file])
        ("Buffers"
         ["Clean up buffer or region" crux-cleanup-buffer-or-region]
         ["Kill other buffers" crux-kill-other-buffers])
        ("Editing"
         ["Go to beginning of line" crux-move-beginning-of-line]
         ["Kill line" crux-smart-kill-line]
         ["Kill whole line" crux-kill-whole-line]
         ["Insert empty line below" crux-smart-open-line]
         ["insert empty line above" crux-smart-open-line-above]
         ["Move up" move-text-up]
         ["Move down" move-text-down]
         ["Duplicate line or region" crux-duplicate-current-line-or-region]
         ["Indent rigidly and copy to clipboard" crux-indent-rigidly-and-copy-to-clipboard]
         ["Indent defun" crux-indent-defun]
         ["Insert date" crux-insert-date]
         ["Eval and replace" crux-eval-and-replace])
        ("Windows"
         ["Swap windows" crux-swap-windows])
        ("General"
         ["Visit term buffer" crux-visit-term-buffer]
         ["Search in Google" prelude-google]
         ["View URL" crux-view-url])))
    map)
  "Keymap for Prelude mode.")

;; define minor mode
(define-minor-mode prelude-mode
  "Minor mode to consolidate Emacs Prelude extensions.

\\{prelude-mode-map}"
  :lighter " Pre"
  :keymap prelude-mode-map
  :global t)

(provide 'prelude-mode)
;;; prelude-mode.el ends here
