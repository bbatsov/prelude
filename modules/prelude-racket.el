;;; prelude-racket.el --- Emacs Prelude: Racket programming support.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Xiongfei Shi <xiongfei.shi@icloud.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Basic configuration for Racket programming.

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

(require 'prelude-lisp)

(declare-function racket-unicode-input-method-enable "racket-input")

(defun prelude-racket-enable-input-mode ()
  "Enable the input method for Unicode symbols (e.g., λ, →, ≤)."
  (if (fboundp 'racket-input-mode)      ; racket-mode 2024-10-15+
      (racket-input-mode +1)
    (with-suppressed-warnings ((obsolete racket-unicode-input-method-enable))
      (racket-unicode-input-method-enable))))

(defun prelude-racket-setup ()
  "Set up a Racket buffer."
  (prelude-lisp-coding-setup)
  (prelude-racket-enable-input-mode)
  (run-hooks 'prelude-racket-mode-hook))

(make-obsolete-variable 'prelude-racket-mode-hook
                        "use `racket-mode-hook' instead."
                        "2.2.0")

;; IDE-like Racket support with REPL, docs, and macro expansion
(use-package racket-mode
  :ensure t
  :bind (:map racket-mode-map
              ("M-RET" . racket-run))
  :hook (racket-repl-mode . prelude-racket-enable-input-mode))

(add-hook 'racket-mode-hook #'prelude-racket-setup -50)

(provide 'prelude-racket)
;;; prelude-racket.el ends here
