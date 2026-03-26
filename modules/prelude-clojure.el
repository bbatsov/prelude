;;; prelude-clojure.el --- Emacs Prelude: Clojure programming configuration.
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A basic setup for Clojure programming based on clojure-mode
;; and CIDER.

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

(defun prelude-clojure-mode-defaults ()
  (subword-mode +1)
  (run-hooks 'prelude-lisp-coding-hook))

(defun prelude-cider-repl-mode-defaults ()
  (subword-mode +1)
  (run-hooks 'prelude-interactive-lisp-coding-hook))

;; For tree-sitter support on Emacs 30+, install clojure-ts-mode
;; separately -- it auto-remaps clojure-mode buffers when present.
(use-package clojure-mode
  :ensure t
  :hook (clojure-mode . (lambda ()
                           (run-hooks 'prelude-clojure-mode-hook))))

;; CIDER: Clojure Interactive Development Environment that Rocks
(use-package cider
  :ensure t
  :defer t
  :config
  ;; Enable to log all nREPL messages for debugging
  ;; (setq nrepl-log-messages t)
  :hook (cider-repl-mode . (lambda ()
                              (run-hooks 'prelude-cider-repl-mode-hook))))

(setq prelude-clojure-mode-hook 'prelude-clojure-mode-defaults)
(setq prelude-cider-repl-mode-hook 'prelude-cider-repl-mode-defaults)

(provide 'prelude-clojure)

;;; prelude-clojure.el ends here
