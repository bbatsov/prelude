;;; prelude-ocaml.el --- Emacs Prelude: decent Perl coding settings.
;;
;; Copyright © 2014-2016 Geoff Shannon
;;
;; Author: Geoff Shannon <geoffpshannon@gmail.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; tuareg is the preferred ocaml mode for Emacs

;; These setups for ocaml assume that you are using the OPAM package
;; manager (http://opam.ocaml.org/).

;; Because of the apparent complexity of getting emacs environment
;; variables setup to use opam correctly, it is instead easier to use
;; opam itself to execute any necessary commands.

;; Also, the standard OCaml toplevel usage has been replaced in favor
;; of UTOP, the universal toplevel, and we assume that you are using
;; the Jane Street Core libraries rather than the regular OCaml
;; standard libraries

;; The minimum required setup for using Prelude's OCaml setup would be
;; to install OPAM, and then, minimally `opam install core utop'.  A
;; good getting started guide is available at
;; https://github.com/realworldocaml/book/wiki/Installation-Instructions

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

(prelude-require-packages '(tuareg utop merlin flycheck-ocaml))

(require 'tuareg)
(require 'utop)
(require 'merlin)

(setq auto-mode-alist
      (append '(("\\.ml[ily]?\\'" . tuareg-mode)
                ("\\.topml\\'" . tuareg-mode))
              auto-mode-alist))

(with-eval-after-load 'merlin
  ;; Disable Merlin's own error checking
  (setq merlin-error-after-save nil)

  ;; Enable Flycheck checker
  (flycheck-ocaml-setup))

(add-hook 'tuareg-mode-hook #'utop-minor-mode)
(add-hook 'tuareg-mode-hook #'merlin-mode)

(add-hook 'tuareg-mode-hook (lambda ()
                              (progn
                                (define-key tuareg-mode-map (kbd "C-c C-s")
                                  'utop)
                                (setq compile-command
                                      "opam config exec corebuild "))))

;; Setup merlin completions company is used by default in prelude
(add-to-list 'company-backends 'merlin-company-backend)

;; Merlin also offers support for autocomplete, uncomment this next line
;; to activate it.
;; (setq merlin-use-auto-complete-mode t)

(setq utop-command "opam config exec utop -- -emacs"
      merlin-error-after-save nil)

(provide 'prelude-ocaml)

;;; prelude-ocaml.el ends here
