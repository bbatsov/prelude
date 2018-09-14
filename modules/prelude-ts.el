;;; prelude-ts.el --- Emacs Prelude: Typescript programming support.
;;
;; Copyright © 2011-2018 LEE Dongjun
;;
;; Author: LEE Dongjun <redongjun@gmail.com>
;; Version: 1.0.0
;; Keywords: convenience typescript

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for Typescript development.

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
(prelude-require-packages '(tide))

(require 'typescript-mode)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(with-eval-after-load 'typescript-mode
  (defun prelude-ts-mode-defaults ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1))

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)

  (setq prelude-ts-mode-hook 'prelude-ts-mode-defaults)

  (add-hook 'typescript-mode-hook (lambda () (run-hooks 'prelude-ts-mode-hook))))

(provide 'prelude-ts)

;;; prelude-ts.el ends here
