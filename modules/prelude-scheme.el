;;; prelude-scheme.el --- Emacs Prelude: Some defaults for Scheme.
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for Scheme programming.

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

(defun prelude-scheme-mode-defaults ()
  (run-hooks 'prelude-lisp-coding-hook))

;; Geiser: REPL-driven development for Scheme (Guile, Chicken, Chez,
;; Racket, etc.)
(use-package geiser
  :ensure t
  :defer t
  :custom
  ;; Auto-start a REPL for autodoc and completion
  (geiser-mode-start-repl-p t)
  ;; Keep history out of ~/.emacs.d
  (geiser-repl-history-filename
   (expand-file-name "geiser-history" prelude-savefile-dir)))

(setq prelude-scheme-mode-hook 'prelude-scheme-mode-defaults)

(add-hook 'scheme-mode-hook (lambda ()
                              (run-hooks 'prelude-scheme-mode-hook)))

(provide 'prelude-scheme)

;;; prelude-scheme.el ends here
