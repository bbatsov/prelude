;;; prelude-erlang.el --- Emacs Prelude: Erlang programming support.
;;
;; Copyright © 2011-2026 Gleb Peregud
;;
;; Author: Gleb Peregud <gleber.p@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Erlang is a concurrent functional language.

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
(prelude-require-packages '(erlang))

(defcustom wrangler-path nil
  "The location of wrangler elisp directory."
  :group 'prelude-erlang
  :type 'string
  :safe 'stringp)

(when prelude-projectile
  (require 'projectile))

(when (require 'erlang-start nil t)
  (when (not (null wrangler-path))
    (add-to-list 'load-path wrangler-path)
    (require 'wrangler)))

(defun prelude-erlang-mode-defaults ()
  (subword-mode +1)
  (prelude-lsp-enable)
  (when prelude-projectile
    (setq erlang-compile-function 'projectile-compile-project)))

(setq prelude-erlang-mode-hook 'prelude-erlang-mode-defaults)

(add-hook 'erlang-mode-hook (lambda ()
                              (run-hooks 'prelude-erlang-mode-hook)))

(provide 'prelude-erlang)

;;; prelude-erlang.el ends here
