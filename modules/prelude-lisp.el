;;; prelude-lisp.el --- Emacs Prelude: Configuration common to all lisp modes.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configuration shared between all modes related to lisp-like languages.

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

(require 'prelude-programming)
(use-package rainbow-delimiters :ensure t :defer t)

;; Lisp configuration
(define-key read-expression-map (kbd "TAB") 'completion-at-point)

;; wrap keybindings
(when prelude-smartparens
  (define-key lisp-mode-shared-map (kbd "M-(") (prelude-wrap-with "("))
  ;; FIXME: Pick terminal-friendly binding.
  ;;(define-key lisp-mode-shared-map (kbd "M-[") (prelude-wrap-with "["))
  (define-key lisp-mode-shared-map (kbd "M-\"") (prelude-wrap-with "\"")))

;; a great lisp coding hook
(defun prelude-lisp-coding-defaults ()
  (when prelude-smartparens
    (smartparens-strict-mode +1))
  (rainbow-delimiters-mode +1))

(add-hook 'prelude-lisp-coding-hook #'prelude-lisp-coding-defaults)

;; interactive modes don't need whitespace checks
(defun prelude-interactive-lisp-coding-defaults ()
  (when prelude-smartparens
    (smartparens-strict-mode +1))
  (rainbow-delimiters-mode +1)
  (whitespace-mode -1))

(add-hook 'prelude-interactive-lisp-coding-hook #'prelude-interactive-lisp-coding-defaults)

(provide 'prelude-lisp)

;;; prelude-lisp.el ends here
