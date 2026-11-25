;;; prelude-programming.el --- Emacs Prelude: prog-mode configuration  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic prog-mode configuration and programming related utilities.

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

(defun prelude-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

;; show the name of the current function definition in the modeline
(which-function-mode 1)

;; font-lock annotations like TODO in source code
(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode 1))

(when prelude-smartparens
  ;; smart curly braces
  (sp-pair "{" nil :post-handlers
           '(((lambda (&rest _ignored)
                (crux-smart-open-line-above)) "RET"))))

;; enlist a more liberal guru
(use-package guru-mode
  :ensure t
  :defer t
  :init
  (setq guru-warn-only t))

(defun prelude-prog-setup ()
  "Set up a buffer of any programming language."
  (when (and prelude-flyspell
             (eq prelude-spell-checker 'flyspell)
             (executable-find ispell-program-name))
    (flyspell-prog-mode))
  (when prelude-guru
    (guru-mode +1)
    (diminish 'guru-mode))
  ;; auto-pairing of delimiters, the built-in way when smartparens is off
  (if prelude-smartparens
      (smartparens-mode +1)
    (electric-pair-local-mode +1))
  (prelude-enable-whitespace)
  (prelude-local-comment-auto-fill)
  (run-hooks 'prelude-prog-mode-hook))

(make-obsolete-variable 'prelude-prog-mode-hook
                        "use `prog-mode-hook' instead."
                        "2.2.0")

;; Programming major modes derive from prog-mode, so that's where the
;; setup above goes.  It runs ahead of the functions you add to
;; `prog-mode-hook' (as do the setups of the language modules on the
;; hooks of their modes), so that those can change anything it did.
(add-hook 'prog-mode-hook #'prelude-prog-setup -50)

;; enable on-the-fly syntax checking
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode +1)
  ;; When Eglot is the LSP client, route its diagnostics through Flycheck
  ;; as well.  On its own Eglot reports only via Flymake, so without this
  ;; LSP diagnostics wouldn't show up in Prelude's Flycheck UI.
  ;; (lsp-mode has its own Flycheck integration, so this is Eglot-only.)
  (when (eq prelude-lsp-client 'eglot)
    ;; Flycheck has had the `eglot-check' checker since version 39.  It
    ;; used to take the flycheck-eglot package.
    (if (flycheck-valid-checker-p 'eglot-check)
        (global-flycheck-eglot-mode +1)
      (display-warning
       'prelude
       "This Flycheck is too old to show the diagnostics of language servers.
Update your packages with M-x prelude-update and restart Emacs."))))

;; Makefiles require tabs for indentation
(defun prelude-makefile-setup ()
  "Set up a Makefile buffer."
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t)
  (run-hooks 'prelude-makefile-mode-hook))

(make-obsolete-variable 'prelude-makefile-mode-hook
                        "use `makefile-mode-hook' instead."
                        "2.2.0")

(add-hook 'makefile-mode-hook #'prelude-makefile-setup -50)

(provide 'prelude-programming)
;;; prelude-programming.el ends here
