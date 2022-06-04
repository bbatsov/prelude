;;; prelude-literate-programming.el --- Emacs Prelude: Literate Programming Support
;;
;; Author: Koustubh Sinkar
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

(defvar prelude-ipynb-packages
  '(code-cells   ; file mode for code-cells
    ein          ; Emacs Ipython Notebook (Jupyter Client)
    elpy         ; emacs python development environment
    ))

(defvar prelude-ob-packages
  '(ob-async   ; asynchronous execution of code-blocks
    ob-ipython ; for python and ipython
    ob-tmux    ; for shell
    ob-deno    ; for javascript
    ob-typescript
    ))


(prelude-require-packages
 (append prelude-ipynb-packages prelude-ob-packages))

(setq prelude-ob-loader-list
  '((python . t)
    (ipython . t)
    (shell . t)
    (js . t)
    (typescript . t)
    ;; Include other languages here...
    ))

;; Run/highlight code using babel in org-mode
(org-babel-do-load-languages
 'org-babel-load-languages prelude-ob-loader-list)

;; Syntax highlight in #+BEGIN_SRC blocks
(setq org-src-fontify-natively t)

;; Don't prompt before running code in org
(setq org-confirm-babel-evaluate nil)

;; Fix an incompatibility between the ob-async and ob-ipython packages
(setq ob-async-no-async-languages-alist '("ipython"))

(defvar org-babel-language-list
  '(ob-cfengine3
    ob-clojurescript
    ob-coffee
    ob-dao
    ob-diagrams
    ob-elixir
    ob-elm
    ob-go
    ob-graphql
    ob-http
    ob-ipython
    ob-julia-vterm
    ob-kotlin
    ob-mongo
    ob-prolog
    ob-restclient
    ob-rust
    ob-sml
    ob-sql-mode
    ob-translate
    ob-typescript
    ob-uart
    ))
;;; TODO Write a function to enable org-babel for each function

(provide 'prelude-literate-programming)
;;; prelude-literate-programming.el ends here
