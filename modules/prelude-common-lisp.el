;;; prelude-common-lisp.el --- Emacs Prelude: lisp-mode and SLIME config.
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configuration for lisp-mode and SLIME.

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

;; the SBCL configuration file is in Common Lisp
(add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))

;; Open files with .cl extension in lisp-mode
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

(add-hook 'lisp-mode-hook (lambda () (run-hooks 'prelude-lisp-coding-hook)))

;; SLIME: Superior Lisp Interaction Mode for Emacs.
;; If you prefer Sly (a modernized SLIME fork), install it in your
;; personal config instead.
(use-package slime
  :ensure t
  :defer t
  :config
  ;; Known Common Lisp implementations.  Use M-- M-x slime to pick one.
  (setq slime-lisp-implementations
        '((ccl ("ccl"))
          (clisp ("clisp" "-q"))
          (cmucl ("cmucl" "-quiet"))
          (sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)))

  (setq slime-default-lisp 'sbcl)

  ;; slime-fancy loads most popular contribs in one go;
  ;; slime-cl-indent provides better CL-aware indentation
  (setq slime-contribs '(slime-fancy slime-cl-indent))

  (setq slime-complete-symbol-function 'slime-flex-completions
        slime-autodoc-use-multiline-p t)

  ;; Uncomment to let the Lisp process evaluate Emacs Lisp.
  ;; Useful for advanced setups but a potential security risk.
  ;; (setq slime-enable-evaluate-in-emacs t)

  ;; rainbow-delimiters messes up colors in slime-repl, so we
  ;; configure the REPL hooks directly instead of using
  ;; prelude-lisp-coding-defaults.
  (add-hook 'slime-repl-mode-hook (lambda ()
                                    (smartparens-strict-mode +1)
                                    (whitespace-mode -1)))

  (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector))

(provide 'prelude-common-lisp)

;;; prelude-common-lisp.el ends here
