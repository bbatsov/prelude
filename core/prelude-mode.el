;;; prelude-mode.el --- Emacs Prelude: minor mode
;;
;; Copyright © 2011-2015 Bozhidar Batsov
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

(defvar prelude-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c o") 'prelude-open-with)
    (define-key map (kbd "C-c g") 'prelude-google)
    (define-key map (kbd "C-c G") 'prelude-github)
    (define-key map (kbd "C-c y") 'prelude-youtube)
    (define-key map (kbd "C-c U") 'prelude-duckduckgo)
    ;; mimic popular IDEs binding, note that it doesn't work in a terminal session
    (define-key map [(shift return)] 'prelude-smart-open-line)
    (define-key map (kbd "M-o") 'prelude-smart-open-line)
    (define-key map [(control shift return)] 'prelude-smart-open-line-above)
    (define-key map [(control shift up)]  'move-text-up)
    (define-key map [(control shift down)]  'move-text-down)
    (define-key map [(meta shift up)]  'move-text-up)
    (define-key map [(meta shift down)]  'move-text-down)
    (define-key map (kbd "C-c n") 'prelude-cleanup-buffer-or-region)
    (define-key map (kbd "C-c f")  'prelude-recentf-ido-find-file)
    (define-key map (kbd "C-M-z") 'prelude-indent-defun)
    (define-key map (kbd "C-c u") 'prelude-view-url)
    (define-key map (kbd "C-c e") 'prelude-eval-and-replace)
    (define-key map (kbd "C-c s") 'prelude-swap-windows)
    (define-key map (kbd "C-c D") 'prelude-delete-file-and-buffer)
    (define-key map (kbd "C-c d") 'prelude-duplicate-current-line-or-region)
    (define-key map (kbd "C-c M-d") 'prelude-duplicate-and-comment-current-line-or-region)
    (define-key map (kbd "C-c r") 'prelude-rename-buffer-and-file)
    (define-key map (kbd "C-c t") 'prelude-visit-term-buffer)
    (define-key map (kbd "C-c k") 'prelude-kill-other-buffers)
    (define-key map (kbd "C-c TAB") 'prelude-indent-rigidly-and-copy-to-clipboard)
    (define-key map (kbd "C-c I") 'prelude-find-user-init-file)
    (define-key map (kbd "C-c S") 'prelude-find-shell-init-file)
    (define-key map (kbd "C-c i") 'prelude-goto-symbol)
    ;; extra prefix for projectile
    (define-key map (kbd "s-p") 'projectile-command-map)
    ;; make some use of the Super key
    (define-key map (kbd "s-g") 'god-local-mode)
    (define-key map (kbd "s-r") 'prelude-recentf-ido-find-file)
    (define-key map (kbd "s-j") 'prelude-top-join-line)
    (define-key map (kbd "s-k") 'prelude-kill-whole-line)
    (define-key map (kbd "s-m m") 'magit-status)
    (define-key map (kbd "s-m l") 'magit-log)
    (define-key map (kbd "s-m f") 'magit-log-buffer-file)
    (define-key map (kbd "s-m b") 'magit-blame)
    (define-key map (kbd "s-o") 'prelude-smart-open-line-above)

    map)
  "Keymap for Prelude mode.")

(defun prelude-mode-add-menu ()
  "Add a menu entry for `prelude-mode' under Tools."
  (easy-menu-add-item nil '("Tools")
                      '("Prelude"
                        ("Files"
                         ["Open with..." prelude-open-with]
                         ["Delete file and buffer" prelude-delete-file-and-buffer]
                         ["Rename buffer and file" prelude-rename-buffer-and-file])

                        ("Buffers"
                         ["Clean up buffer or region" prelude-cleanup-buffer-or-region]
                         ["Kill other buffers" prelude-kill-other-buffers])

                        ("Editing"
                         ["Insert empty line" prelude-insert-empty-line]
                         ["Move line up" prelude-move-line-up]
                         ["Move line down" prelude-move-line-down]
                         ["Duplicate line or region" prelude-duplicate-current-line-or-region]
                         ["Indent rigidly and copy to clipboard" prelude-indent-rigidly-and-copy-to-clipboard]
                         ["Insert date" prelude-insert-date]
                         ["Eval and replace" prelude-eval-and-replace]
                         )

                        ("Windows"
                         ["Swap windows" prelude-swap-windows])

                        ("General"
                         ["Visit term buffer" prelude-visit-term-buffer]
                         ["Search in Google" prelude-google]
                         ["View URL" prelude-view-url]))
                      "Search Files (Grep)...")

  (easy-menu-add-item nil '("Tools") '("--") "Search Files (Grep)..."))

(defun prelude-mode-remove-menu ()
  "Remove `prelude-mode' menu entry."
  (easy-menu-remove-item nil '("Tools") "Prelude")
  (easy-menu-remove-item nil '("Tools") "--"))

;; define minor mode
(define-minor-mode prelude-mode
  "Minor mode to consolidate Emacs Prelude extensions.

\\{prelude-mode-map}"
  :lighter " Pre"
  :keymap prelude-mode-map
  (if prelude-mode
      ;; on start
      (prelude-mode-add-menu)
    ;; on stop
    (prelude-mode-remove-menu)))

(define-globalized-minor-mode prelude-global-mode prelude-mode prelude-on)

(defun prelude-on ()
  "Turn on `prelude-mode'."
  (prelude-mode +1))

(defun prelude-off ()
  "Turn off `prelude-mode'."
  (prelude-mode -1))

(provide 'prelude-mode)
;;; prelude-mode.el ends here
