;;; prelude-fsharp.el --- Emacs Prelude: F# programming support.
;;
;; Author: Andre Boechat <andre.boechat@tutanota.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Basic setup for F# programming based on fsharp-mode and Eglot.

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
(prelude-require-packages '(fsharp-mode eglot-fsharp))

(with-eval-after-load 'fsharp-mode
  (defun prelude-fsharp-mode-defaults ()
    ;; A reasonable default path to the F# compiler and interpreter on
    ;; Unix-like systems.
    ;; https://github.com/fsharp/emacs-fsharp-mode#compiler-and-repl-paths
    (setq inferior-fsharp-program "dotnet fsi --readline-")
    (require 'eglot-sharp))

  (setq prelude-fsharp-mode-hook 'prelude-fsharp-mode-defaults)

  (add-hook 'fsharp-mode-hook (lambda ()
                                (run-hooks 'prelude-sharp-mode-hook))))

(provide 'prelude-fsharp)

;;; prelude-fsharp.el ends here
