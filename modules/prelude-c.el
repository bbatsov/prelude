;;; prelude-c.el --- Emacs Prelude: cc-mode configuration.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for cc-mode and the modes derived from it.

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

(declare-function c-add-style "cc-styles")

;; Use tree-sitter modes when grammars are available
(prelude-treesit-remap 'c 'c-mode 'c-ts-mode)
(prelude-treesit-remap 'cpp 'c++-mode 'c++-ts-mode)

(defun prelude-c--set-default (variable value)
  "Set VARIABLE to VALUE, unless it's been set to something else already.
That's anything other than its standard value, be it in your personal
config, in personal/preload or with Customize."
  (let ((variable (indirect-variable variable)))
    (when (equal (default-value variable)
                 (eval (car (get variable 'standard-value)) t))
      (set-default variable value))))

;; K&R style with an indentation of 4 for C, C++ and Objective-C.  A
;; buffer's style is picked when its mode starts, so this has to be in
;; place before that, and not in a mode hook.

;; the tree-sitter modes (the offset is `c-ts-indent-offset' in Emacs 31)
(with-eval-after-load 'c-ts-mode
  (prelude-c--set-default 'c-ts-mode-indent-style 'k&r)
  (prelude-c--set-default 'c-ts-mode-indent-offset 4))

;; the classic modes, which have no K&R style with an indentation of 4
(with-eval-after-load 'cc-styles
  (c-add-style "prelude" '("k&r" (c-basic-offset . 4))))
;; the other modes built on cc-mode keep their usual styles
(with-eval-after-load 'cc-vars
  (prelude-c--set-default 'c-default-style
                          '((c-mode . "prelude")
                            (c++-mode . "prelude")
                            (objc-mode . "prelude")
                            (java-mode . "java")
                            (awk-mode . "awk")
                            (other . "gnu"))))

(defun prelude-c-mode-common-defaults ()
  "Default coding hook, useful with C and C++."
  (subword-mode +1)
  (prelude-lsp-enable))

(add-hook 'prelude-c-mode-common-hook #'prelude-c-mode-common-defaults)

(defun prelude-c-setup ()
  "Run `prelude-c-mode-common-hook'."
  (run-hooks 'prelude-c-mode-common-hook))

;; Only for C, C++ and Objective-C: the other modes built on cc-mode
;; (Java, C#, PHP, etc.) are set up by modules of their own.  This runs
;; ahead of the functions you add to the same hooks, so that those can
;; change what it did.
(dolist (hook '(c-mode-hook c++-mode-hook objc-mode-hook c-ts-mode-hook c++-ts-mode-hook))
  (add-hook hook #'prelude-c-setup -50))

;; Major mode for CMake build files
(use-package cmake-mode
  :ensure t
  :defer t)

(provide 'prelude-c)

;;; prelude-c.el ends here
