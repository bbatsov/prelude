;;; prelude-perl.el --- Emacs Prelude: decent Perl coding settings.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; cperl-mode is the best Perl mode for Emacs out there.

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

(defvar cperl-indent-level)
(defvar cperl-continued-statement-offset)
(defvar cperl-font-lock)
(defvar cperl-electric-lbrace-space)
(defvar cperl-electric-parens)
(defvar cperl-electric-linefeed)
(defvar cperl-electric-keywords)
(defvar cperl-info-on-command-no-prompt)
(defvar cperl-clobber-lisp-bindings)
(defvar cperl-lazy-help-time)
(defvar cperl-invalid-face)

(require 'prelude-programming)

;; Prefer cperl-mode over perl-mode for all Perl files
(add-to-list 'major-mode-remap-alist '(perl-mode . cperl-mode))

;; C-h P to look up Perl documentation
(define-key 'help-command (kbd "P") 'cperl-perldoc)

(defun prelude-cperl-mode-defaults ()
  (setq cperl-indent-level 4)
  (setq cperl-continued-statement-offset 8)
  ;; Fine-grained control over electric behavior instead of the
  ;; all-or-nothing cperl-hairy option
  (setq cperl-font-lock t)
  (setq cperl-electric-lbrace-space t)
  (setq cperl-electric-parens nil)
  (setq cperl-electric-linefeed nil)
  (setq cperl-electric-keywords nil)
  (setq cperl-info-on-command-no-prompt t)
  (setq cperl-clobber-lisp-bindings t)
  (setq cperl-lazy-help-time 3)

  ;; Remove distracting background colors on array/hash variables
  (set-face-background 'cperl-array-face nil)
  (set-face-background 'cperl-hash-face nil)
  ;; Don't highlight trailing whitespace (Emacs 30+ cperl-mode follows
  ;; `show-trailing-whitespace' instead, which is off by default)
  (when (< emacs-major-version 30)
    (with-suppressed-warnings ((obsolete cperl-invalid-face))
      (setq cperl-invalid-face nil)))
  (subword-mode +1))

(add-hook 'prelude-cperl-mode-hook #'prelude-cperl-mode-defaults)

(add-hook 'cperl-mode-hook (lambda ()
                             (run-hooks 'prelude-cperl-mode-hook)) t)

(provide 'prelude-perl)

;;; prelude-perl.el ends here
