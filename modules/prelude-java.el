;;; prelude-java.el --- Emacs Prelude: Java programming support.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Prelude configuration for Java, built around the Eclipse JDT language
;; server.  All you need on your system is a JDK - the language server
;; is downloaded the first time you open a Java file.

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

(declare-function dap-java-run-test-method "dap-java")
(declare-function dap-java-debug "dap-java")

;; Use java-ts-mode when the tree-sitter grammar is available
(prelude-treesit-remap 'java 'java-mode 'java-ts-mode)

(defun prelude-java-mode-defaults ()
  "Default coding hook, useful with Java."
  ;; CamelCase aware editing operations
  (subword-mode +1)
  (pcase prelude-lsp-client
    ('eglot
     ;; eglot-java-mode starts Eglot itself, after downloading the
     ;; language server if it's missing.  That shouldn't happen for
     ;; buffers that aren't visiting a file (e.g. when Org fontifies a
     ;; source block), and a failed download shouldn't break the mode.
     (when (and buffer-file-name (fboundp 'eglot-java-mode))
       (with-demoted-errors "[Prelude] Couldn't start the Java language server: %S"
         (eglot-java-mode +1))))
    ('lsp-mode
     (prelude-lsp-enable)
     (local-set-key (kbd "C-c C-t") #'dap-java-run-test-method)
     (local-set-key (kbd "C-c C-r") #'dap-java-debug))))

;; Eglot's Java support is bare-bones: it expects a jdtls executable on
;; the PATH and can't open the classes of your dependencies.  eglot-java
;; installs the language server, handles the jdt:// URIs it uses for
;; library classes, runs tests and main classes, and creates projects.
(when (eq prelude-lsp-client 'eglot)
  (use-package eglot-java
    :ensure t
    :bind (:map eglot-java-mode-map
                ("C-c C-t" . eglot-java-run-test)
                ("C-c C-r" . eglot-java-run-main))))

;; lsp-java installs the language server as well, and comes with
;; dap-java for running tests and debugging.
(when (eq prelude-lsp-client 'lsp-mode)
  (use-package lsp-java
    :ensure t
    :defer t)

  (use-package dap-java
    :ensure nil
    :commands (dap-java-run-test-method dap-java-debug)))

(add-hook 'prelude-java-mode-hook #'prelude-java-mode-defaults)

(add-hook 'java-mode-hook (lambda ()
                            (run-hooks 'prelude-java-mode-hook)))
(add-hook 'java-ts-mode-hook (lambda ()
                               (run-hooks 'prelude-java-mode-hook)))

(provide 'prelude-java)
;;; prelude-java.el ends here
