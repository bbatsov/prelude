;;; prelude-racket.el --- Emacs Prelude: Racket programming support.
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

(defun prelude-racket-mode-defaults ()
  (run-hooks 'prelude-lisp-coding-hook)
  ;; Input method for Unicode symbols (e.g., λ, →, ≤)
  (racket-unicode-input-method-enable))

;; IDE-like Racket support with REPL, docs, and macro expansion
(use-package racket-mode
  :ensure t
  :bind (:map racket-mode-map
              ("M-RET" . racket-run)
              ("M-." . racket-repl-visit-definition))
  :hook ((racket-mode . (lambda ()
                           (run-hooks 'prelude-racket-mode-hook)))
         (racket-repl-mode . racket-unicode-input-method-enable)))

(setq prelude-racket-mode-hook 'prelude-racket-mode-defaults)

(provide 'prelude-racket)

;;; prelude-racket.el ends here
