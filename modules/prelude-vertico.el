;;; prelude-vertico.el --- Vertico setup  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Vertico-related config.  Vertico is a smart framework for minibuffer
;; completion/filtering/selection (think of ivy/ido).

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

;; Enable vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Smarter path editing in file prompts (ships as part of Vertico):
;; RET descends into the selected directory instead of opening it in
;; Dired, DEL deletes a whole directory component at once, and M-DEL
;; deletes just a word of it.
(use-package vertico-directory
  :ensure nil ; comes with vertico
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; tidy the shadowed part of the path when you re-root it (e.g. type
  ;; ~/ or / in the middle of a path)
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; A few more useful configurations for Vertico
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun prelude-crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'prelude-crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; use the `orderless' completion style.
(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Rich annotations in the minibuffer (docstrings, file sizes, etc.)
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
  :bind (
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ;; Note: M-g e and M-g f are intentionally omitted from consult's
         ;; recommended bindings to avoid clobbering Prelude's long-standing
         ;; avy bindings (avy-goto-word-0 and avy-goto-line).  Bind
         ;; consult-compile-error / consult-flymake in your personal config
         ;; if you'd rather have them.
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines))
  :config
  ;; press < followed by a group key to narrow the candidates to a
  ;; single group (e.g. in consult-buffer, < b shows only buffers);
  ;; press < again to remove the narrowing
  (setq consult-narrow-key "<"))

;; Embark - a keyboard-driven context menu.  Point it at a minibuffer
;; candidate or a thing at point (a file, a URL, a symbol, ...) and it
;; offers the actions that make sense for it.
;;
;; Note: C-. and C-; are also bound by `flyspell-mode' (auto-correct),
;; so in buffers where Prelude enables Flyspell those keys keep their
;; Flyspell meaning; Embark still works everywhere else, including the
;; minibuffer.  We deliberately leave `prefix-help-command' alone so
;; which-key keeps handling the C-h-after-a-prefix help.
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)       ;; act on the thing at point / candidate
         ("C-;" . embark-dwim)      ;; run the default action
         ("C-h B" . embark-bindings))) ;; browse bindings via completing-read

;; Integration between Embark and Consult, e.g. `embark-export' from a
;; consult-ripgrep session into an editable grep buffer.
(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'prelude-vertico)
;;; prelude-vertico.el ends here
