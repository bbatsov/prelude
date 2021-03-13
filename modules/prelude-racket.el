;;; prelude-racket.el --- Emacs Prelude: Racket programming support.
;;
;; Copyright © 2011-2021 Bozhidar Batsov
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

(prelude-require-packages '(racket-mode))

(require 'prelude-lisp)

(with-eval-after-load 'racket-mode
  (define-key racket-mode-map (kbd "M-RET") 'racket-run)
  (define-key racket-mode-map (kbd "M-.") 'racket-repl-visit-definition)

  ;; Enable the common Lisp coding hook
  (add-hook 'racket-mode-hook (lambda () (run-hooks 'prelude-lisp-coding-hook)))

  (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable)
  (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable))

(add-to-list 'auto-mode-alist '("\\.rkt?\\'" . racket-mode))
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))

(provide 'prelude-racket)

;;; prelude-racket ends here
