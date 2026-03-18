;;; prelude-shell.el --- Emacs Prelude: sh-mode configuration.
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for sh-mode.

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

(require 'sh-script)

;; Use bash-ts-mode when the tree-sitter grammar is available
(prelude-treesit-remap 'bash 'sh-mode 'bash-ts-mode)

;; recognize prezto files as zsh scripts
(defvar prelude-prezto-files '("zlogin" "zlogout" "zpreztorc" "zprofile" "zshenv" "zshrc"))

(mapc (lambda (file)
        (add-to-list 'auto-mode-alist `(,(format "\\%s\\'" file) . sh-mode)))
      prelude-prezto-files)

(defun prelude-sh-mode-defaults ()
  (subword-mode +1)
  ;; Auto-detect zsh for prezto files
  (when (and buffer-file-name
             (member (file-name-nondirectory buffer-file-name) prelude-prezto-files))
    (sh-set-shell "zsh")))

(setq prelude-sh-mode-hook 'prelude-sh-mode-defaults)

(add-hook 'sh-mode-hook (lambda ()
                          (run-hooks 'prelude-sh-mode-hook)))
(add-hook 'bash-ts-mode-hook (lambda ()
                               (run-hooks 'prelude-sh-mode-hook)))

(provide 'prelude-shell)
;;; prelude-shell.el ends here
