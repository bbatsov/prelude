;;; prelude-c.el --- Emacs Prelude: cc-mode configuration.
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for cc-mode and the modes derived from it.

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

;; Use tree-sitter modes when grammars are available
(prelude-treesit-remap 'c 'c-mode 'c-ts-mode)
(prelude-treesit-remap 'cpp 'c++-mode 'c++-ts-mode)

(defun prelude-c-mode-common-defaults ()
  ;; K&R style for classic cc-mode
  (setq c-default-style "k&r"
        c-basic-offset 4)
  (c-set-offset 'substatement-open 0)
  ;; Match the K&R style in tree-sitter mode (c-set-offset has no
  ;; effect there since it uses its own indentation engine)
  (when (derived-mode-p 'c-ts-mode 'c++-ts-mode)
    (setq c-ts-mode-indent-style 'k&r))
  (subword-mode +1)
  (prelude-lsp-enable))

(setq prelude-c-mode-common-hook 'prelude-c-mode-common-defaults)

;; this will affect all modes derived from cc-mode, like
;; java-mode, php-mode, etc
(add-hook 'c-mode-common-hook (lambda ()
                                (run-hooks 'prelude-c-mode-common-hook)))
(add-hook 'c-ts-mode-hook (lambda ()
                             (run-hooks 'prelude-c-mode-common-hook)))
(add-hook 'c++-ts-mode-hook (lambda ()
                               (run-hooks 'prelude-c-mode-common-hook)))

;; Major mode for CMake build files
(use-package cmake-mode
  :ensure t
  :defer t)

(provide 'prelude-c)

;;; prelude-c.el ends here
