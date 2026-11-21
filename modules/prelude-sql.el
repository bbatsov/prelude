;;; prelude-sql.el --- Emacs Prelude: SQL programming support.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Prelude configuration for SQL: smarter indentation for the built-in
;; sql-mode and (optional) LSP.

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

(defvar eglot-server-programs)

;; Eglot doesn't know about any SQL language server.
;; (lsp-mode supports sqls out of the box)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(sql-mode . ("sqls"))))

(defun prelude-sql-mode-defaults ()
  "Default coding hook, useful with SQL."
  ;; indent according to the syntax of the statement
  (sqlind-minor-mode +1)
  ;; Plenty of people edit SQL without a language server around.
  (prelude-lsp-enable 'quiet))

(defun prelude-sql-interactive-mode-defaults ()
  "Default hook for SQL consoles."
  ;; wide result sets are unreadable when their rows get wrapped
  (setq truncate-lines t))

;; Syntax-based indentation for SQL
(use-package sql-indent
  :ensure t
  :defer t)

(add-hook 'prelude-sql-mode-hook #'prelude-sql-mode-defaults)
(add-hook 'prelude-sql-interactive-mode-hook #'prelude-sql-interactive-mode-defaults)

(add-hook 'sql-mode-hook (lambda ()
                           (run-hooks 'prelude-sql-mode-hook)))
(add-hook 'sql-interactive-mode-hook (lambda ()
                                       (run-hooks 'prelude-sql-interactive-mode-hook)))

(provide 'prelude-sql)
;;; prelude-sql.el ends here
