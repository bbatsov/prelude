;;; prelude-latex.el --- Emacs Prelude: Sane setup for LaTeX writers.
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Nice defaults for the premium LaTeX editing mode auctex.

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

(require 'smartparens-latex)

;; AUCTeX: the de facto standard LaTeX editing environment for Emacs
(use-package auctex
  :ensure t
  :defer t)

;; Fast math symbol entry via backtick shortcuts (e.g., ` a → \alpha)
(use-package cdlatex
  :ensure t
  :defer t)

;; Company completion for AUCTeX (labels, citations, macros, etc.)
(use-package company-auctex
  :ensure t
  :defer t
  :after company
  :config (company-auctex-init))

(defcustom prelude-latex-fast-math-entry 'LaTeX-math-mode
  "Method used for fast math symbol entry in LaTeX."
  :link '(function-link :tag "AUCTeX Math Mode" LaTeX-math-mode)
  :link '(emacs-commentary-link :tag "CDLaTeX" "cdlatex.el")
  :group 'prelude
  :type '(choice (const :tag "None" nil)
                 (const :tag "AUCTeX Math Mode" LaTeX-math-mode)
                 (const :tag "CDLaTeX" cdlatex)))

;; AUCTeX configuration
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; Disable smart quote insertion -- many users prefer to manage
;; quotes manually or via a snippet system
(setq TeX-close-quote "")
(setq TeX-open-quote "")

;; Ask for the master file when editing multi-file documents
(setq-default TeX-master nil)

;; sensible defaults for macOS, other OSes should be covered out-of-the-box
(when (eq system-type 'darwin)
  (setq TeX-view-program-selection
        '((output-dvi "DVI Viewer")
          (output-pdf "PDF Viewer")
          (output-html "HTML Viewer")))

  (setq TeX-view-program-list
        '(("DVI Viewer" "open %o")
          ("PDF Viewer" "open %o")
          ("HTML Viewer" "open %o"))))

(defun prelude-latex-mode-defaults ()
  "Default Prelude hook for `LaTeX-mode'."
  (turn-on-auto-fill)
  (abbrev-mode +1)
  (subword-mode +1)
  (smartparens-mode +1)
  (pcase prelude-latex-fast-math-entry
    ('LaTeX-math-mode (LaTeX-math-mode 1))
    ('cdlatex (turn-on-cdlatex))))

(setq prelude-latex-mode-hook 'prelude-latex-mode-defaults)

(add-hook 'LaTeX-mode-hook (lambda ()
                             (run-hooks 'prelude-latex-mode-hook)))

(provide 'prelude-latex)

;;; prelude-latex.el ends here
