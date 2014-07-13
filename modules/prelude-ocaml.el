;;; prelude-ocaml.el --- Emacs Prelude: decent Perl coding settings.
;;
;; Copyright © 2014 Geoff Shannon
;;
;; Author: Geoff Shannon <geoffpshannon@gmail.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; tuareg is the preferred ocaml mode for Emacs

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

(prelude-require-packages '(tuareg utop merlin))

(require 'tuareg)
(require 'utop)
(require 'merlin)

(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)

(setq auto-mode-alist
      (append '(("\\.ml[ily]?\\'" . tuareg-mode)
                ("\\.topml\\'" . tuareg-mode))
              auto-mode-alist))

(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)

(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
(add-hook 'tuareg-mode-hook 'merlin-mode)

(setq utop-command "opam config exec \"utop -emacs\""
      merlin-error-after-save nil)

(provide 'prelude-ocaml)

;;; prelude-ocaml.el ends here
