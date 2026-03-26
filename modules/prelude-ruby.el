;;; prelude-ruby.el --- Emacs Prelude: A nice setup for Ruby (and Rails) devs.
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for Ruby and Rails development.

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

;; Use ruby-ts-mode when the tree-sitter grammar is available
(prelude-treesit-remap 'ruby 'ruby-mode 'ruby-ts-mode)

;; We never want to edit Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

(defun prelude-ruby-mode-defaults ()
  ;; Don't auto-insert encoding comments
  ;; Those are almost never needed in Ruby 2+
  (setq ruby-insert-encoding-magic-comment nil)
  (inf-ruby-minor-mode +1)
  ;; CamelCase aware editing operations
  (subword-mode +1)
  (prelude-lsp-enable))

;; Run a Ruby REPL (IRB/Pry) inside Emacs and send code to it
(use-package inf-ruby
  :ensure t
  :defer t)

;; Browse Ruby documentation via ri (C-h R)
(use-package yari
  :ensure t
  :defer t
  :bind (:map help-map ("R" . yari)))

(setq prelude-ruby-mode-hook 'prelude-ruby-mode-defaults)

(add-hook 'ruby-mode-hook (lambda ()
                            (run-hooks 'prelude-ruby-mode-hook)))
(add-hook 'ruby-ts-mode-hook (lambda ()
                               (run-hooks 'prelude-ruby-mode-hook)))

(provide 'prelude-ruby)
;;; prelude-ruby.el ends here
