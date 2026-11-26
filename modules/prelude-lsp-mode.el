;;; prelude-lsp-mode.el --- Emacs Prelude: lsp-mode setup.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov, Ben Alex
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; lsp-mode config.

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

(defvar lsp-ui-mode-map)

(use-package lsp-mode
  :ensure t
  :defer t
  :custom
  ;; read when lsp-mode is loaded, so it has to be set before that
  (lsp-keymap-prefix "C-c L")
  (lsp-keep-workspace-alive nil)
  (lsp-auto-guess-root t)
  (lsp-idle-delay 0.5)
  (lsp-log-io nil)
  (lsp-completion-provider :capf))

;; lsp-mode turns on lsp-ui by itself.  (Not with `:after lsp-mode':
;; that would set the options below only once lsp-mode is loaded,
;; overriding the personal config.)
(use-package lsp-ui
  :ensure t
  :defer t
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-delay 0.5)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-always-show t)
  :config
  (require 'lsp-ui-imenu)
  (define-key lsp-ui-mode-map (kbd "C-c L .") 'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map (kbd "C-c L ?") 'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map (kbd "C-c L r") 'lsp-rename)
  (define-key lsp-ui-mode-map (kbd "C-c L x") 'lsp-workspace-restart)
  (define-key lsp-ui-mode-map (kbd "C-c L w") 'lsp-ui-peek-find-workspace-symbol)
  (define-key lsp-ui-mode-map (kbd "C-c L i") 'lsp-ui-peek-find-implementation)
  (define-key lsp-ui-mode-map (kbd "C-c L d") 'lsp-describe-thing-at-point)
  (define-key lsp-ui-mode-map (kbd "C-c L e") 'lsp-execute-code-action)
  (define-key lsp-ui-mode-map (kbd "C-c L f") 'lsp-format-buffer)
  (define-key lsp-ui-mode-map (kbd "C-c L o") 'lsp-organize-imports)
  (define-key lsp-ui-mode-map (kbd "C-c L m") 'lsp-ui-imenu))

(provide 'prelude-lsp-mode)
;;; prelude-lsp-mode.el ends here
