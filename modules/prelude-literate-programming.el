;;; prelude-literate-programming.el --- Emacs Prelude: Literate Programming Support  -*- lexical-binding: t; -*-
;;
;; Author: Koustubh Sinkar
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: literate programming

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Prelude configuration for literate programming

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

(declare-function ob-ipython-auto-configure-kernels "ob-ipython")
(defvar org-babel-load-languages)
(defvar ob-async-no-async-languages-alist)

(defvar prelude-ipynb-packages
  '(code-cells   ; file mode for code-cells
    ein          ; Emacs Ipython Notebook (Jupyter Client)
    )
  "Packages for working with Jupyter notebooks.")

(defvar prelude-ob-packages
  '(ob-async   ; asynchronous execution of code-blocks
    ob-ipython ; for python and ipython
    ob-tmux    ; for shell
    ob-deno    ; for javascript
    ob-typescript
    )
  "Packages that teach Org Babel more languages, or more tricks.")

(dolist (package (append prelude-ipynb-packages prelude-ob-packages))
  (prelude-use-package-ensure package '(t) nil))

(defvar prelude-ob-loader-list
  '((python . t)
    (ipython . t)
    (shell . t)
    (js . t)
    (deno . t)
    (typescript . t))
  "The languages whose code blocks Org Babel runs.
Set it in personal/preload to change it, as it takes effect when Org
is loaded.")

;; Org is loaded only once you use it
(with-eval-after-load 'org
  ;; Run code blocks using babel in org-mode.  A language whose package
  ;; couldn't be installed is left out, as it would keep the others
  ;; (and with them Org) from loading.
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append (seq-filter (lambda (language)
                         (locate-library (format "ob-%s" (car language))))
                       prelude-ob-loader-list)
           org-babel-load-languages))
  ;; :async code blocks
  (require 'ob-async nil t))

;; ob-ipython queries Jupyter for its kernels whenever an Org buffer is
;; opened, which fails (and breaks `org-mode') if Jupyter isn't installed.
(with-eval-after-load 'ob-ipython
  (unless (executable-find "jupyter")
    (remove-hook 'org-mode-hook #'ob-ipython-auto-configure-kernels)))

;; Fix an incompatibility between the ob-async and ob-ipython packages
(setq ob-async-no-async-languages-alist '("ipython"))

(provide 'prelude-literate-programming)
;;; prelude-literate-programming.el ends here
