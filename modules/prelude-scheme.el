;;; prelude-scheme.el --- Emacs Prelude: Some defaults for Scheme.
;;
;; Copyright © 2011-2015 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

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
(prelude-require-package 'geiser)

(require 'prelude-lisp)
(require 'geiser)

;; geiser replies on a REPL to provide autodoc and completion
(setq geiser-mode-start-repl-p t)

;; keep the home clean
(setq geiser-repl-history-filename
      (expand-file-name "geiser-history" prelude-savefile-dir))

(add-hook 'scheme-mode-hook (lambda () (run-hooks 'prelude-lisp-coding-hook)))

(provide 'prelude-scheme)

;;; prelude-scheme.el ends here
