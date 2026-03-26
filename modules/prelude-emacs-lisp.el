;;; prelude-emacs-lisp.el --- Emacs Prelude: Nice config for Elisp programming.
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Nice config for Elisp Programming.

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
(require 'crux)

(defun prelude-recompile-elc-on-save ()
  "Recompile your elc when saving an elisp file."
  (add-hook 'after-save-hook
            (lambda ()
              (when (and
                     (string-prefix-p prelude-dir (file-truename buffer-file-name))
                     (file-exists-p (byte-compile-dest-file buffer-file-name)))
                (emacs-lisp-byte-compile)))
            nil
            t))

(defun prelude-visit-ielm ()
  "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
  (interactive)
  (crux-start-or-switch-to 'ielm "*ielm*"))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'prelude-visit-ielm)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

(defun prelude-conditional-emacs-lisp-checker ()
  "Don't check doc style in Emacs Lisp test files."
  (let ((file-name (buffer-file-name)))
    (when (and file-name (string-match-p ".*-tests?\\.el\\'" file-name))
      (setq-local flycheck-checkers '(emacs-lisp)))))

(defun prelude-emacs-lisp-mode-defaults ()
  "Sensible defaults for `emacs-lisp-mode'."
  (run-hooks 'prelude-lisp-coding-hook)
  (eldoc-mode +1)
  (prelude-recompile-elc-on-save)
  (rainbow-mode +1)
  (setq mode-name "EL")
  (prelude-conditional-emacs-lisp-checker))

(setq prelude-emacs-lisp-mode-hook 'prelude-emacs-lisp-mode-defaults)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (run-hooks 'prelude-emacs-lisp-mode-hook)))

;; Recognize Emacs package build tool files as Emacs Lisp
(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("Eask\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("Eldev\\'" . emacs-lisp-mode))

;; ielm is an interactive Emacs Lisp shell
(defun prelude-ielm-mode-defaults ()
  "Sensible defaults for `ielm'."
  (run-hooks 'prelude-interactive-lisp-coding-hook)
  (eldoc-mode +1))

(setq prelude-ielm-mode-hook 'prelude-ielm-mode-defaults)

(add-hook 'ielm-mode-hook (lambda ()
                            (run-hooks 'prelude-ielm-mode-hook)))

;; M-. to jump to Elisp definition, C-c C-d to describe symbol at
;; point.  Complements built-in xref with Elisp-specific navigation.
(use-package elisp-slime-nav
  :ensure t
  :diminish
  :hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode))

;; Colorize color names and hex values in Elisp buffers
(use-package rainbow-mode
  :ensure t
  :diminish
  :defer t)

(with-eval-after-load "eldoc"
  (diminish 'eldoc-mode))

(with-eval-after-load "ielm"
  (define-key ielm-map (kbd "M-(") (prelude-wrap-with "("))
  (define-key ielm-map (kbd "M-\"") (prelude-wrap-with "\"")))

(defun prelude-conditionally-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (smartparens-mode 1)))

(add-hook 'minibuffer-setup-hook 'prelude-conditionally-enable-smartparens-mode)

(provide 'prelude-emacs-lisp)

;;; prelude-emacs-lisp.el ends here
