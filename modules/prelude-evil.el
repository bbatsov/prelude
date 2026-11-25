;;; prelude-evil.el --- Emacs Prelude: evil-mode configuration.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for evil-mode.

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

;; The commands below are bound in evil's keymaps.

(defun prelude-yank-to-end-of-line ()
  "Yank to end of line."
  (interactive)
  (evil-yank (point) (line-end-position)))

(defun prelude-shift-left-visual ()
  "Shift left and restore visual selection."
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun prelude-shift-right-visual ()
  "Shift right and restore visual selection."
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

;; Scrolling
(defun prelude-evil-scroll-down-other-window ()
  (interactive)
  (scroll-other-window))

(defun prelude-evil-scroll-up-other-window ()
  (interactive)
  (scroll-other-window '-))

;;; snagged from Eric S. Fraga
;;; http://lists.gnu.org/archive/html/emacs-orgmode/2012-05/msg00153.html
(defun prelude-evil-key-bindings-for-org ()
  (evil-declare-key 'normal org-mode-map
    "gk" 'outline-up-heading
    "gj" 'outline-next-visible-heading
    "H" 'org-beginning-of-line ; smarter behaviour on headlines etc.
    "L" 'org-end-of-line ; smarter behaviour on headlines etc.
    "t" 'org-todo ; mark a TODO item as DONE
    ",c" 'org-cycle
    (kbd "TAB") 'org-cycle
    ",e" 'org-export-dispatch
    ",n" 'outline-next-visible-heading
    ",p" 'outline-previous-visible-heading
    ",t" 'org-set-tags-command
    ",u" 'outline-up-heading
    "$" 'org-end-of-line ; smarter behaviour on headlines etc.
    "^" 'org-beginning-of-line ; ditto
    "-" 'org-ctrl-c-minus ; change bullet style
    "<" 'org-metaleft ; out-dent
    ">" 'org-metaright ; indent
    ))

;; vim style numeric incrementing and decrementing (bound below)
(use-package evil-numbers
  :ensure t
  :defer t)

;; g; and g, go to recent changes
(use-package goto-chg
  :ensure t
  :defer t)

(use-package evil
  :ensure t
  :custom
  (evil-mode-line-format 'before)
  ;; prevent esc-key from translating to meta-key in terminal mode
  (evil-esc-delay 0)
  (evil-shift-width 2)
  :bind (:map evil-normal-state-map
              ("Y" . prelude-yank-to-end-of-line)
              ("C-S-d" . prelude-evil-scroll-down-other-window)
              ("C-S-u" . prelude-evil-scroll-up-other-window)
              ;; vim style numeric incrementing and decrementing
              ("C-A" . evil-numbers/inc-at-pt)
              ("C-S-A" . evil-numbers/dec-at-pt)
              ;; enable avy with evil-mode
              ("SPC" . avy-goto-word-1)
              :map evil-visual-state-map
              (">" . prelude-shift-right-visual)
              ("<" . prelude-shift-left-visual))
  :init
  ;; Evil makes these with `defvar' when it defines its states, so they
  ;; aren't options that `:custom' could set before it's loaded
  (setq evil-emacs-state-cursor '("red" box)
        evil-normal-state-cursor '("gray" box)
        evil-visual-state-cursor '("gray" box)
        evil-insert-state-cursor '("gray" bar)
        evil-motion-state-cursor '("gray" box))
  (evil-mode 1)
  :config
  ;; Other useful Commands
  (evil-ex-define-cmd "W"     'evil-write-all)
  (evil-ex-define-cmd "Tree"  'speedbar-get-focus)
  (evil-ex-define-cmd "linum" 'display-line-numbers-mode)
  (evil-ex-define-cmd "Align" 'align-regexp)

  ;; Magit from avsej
  (evil-add-hjkl-bindings magit-log-mode-map 'emacs)
  (evil-add-hjkl-bindings magit-status-mode-map 'emacs
    "K" 'magit-discard
    "l" 'magit-log
    "h" 'magit-diff-toggle-refine-hunk)

  (prelude-evil-key-bindings-for-org))

;; add, change and delete the delimiters around things
(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

;; * and # in visual state search for the selection
(use-package evil-visualstar
  :ensure t
  :after evil
  :config
  (global-evil-visualstar-mode 1))

(provide 'prelude-evil)
;;; prelude-evil.el ends here
