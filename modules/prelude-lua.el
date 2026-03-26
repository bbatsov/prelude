;;; prelude-lua.el --- Emacs Prelude: Lua programming configuration.
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Xiongfei Shi <xiongfei.shi@icloud.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Basic configuration for Lua programming.  Install lua-language-server
;; for LSP support:
;;   https://github.com/LuaLS/lua-language-server

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

(defun prelude-lua-mode-defaults ()
  (subword-mode +1)
  (prelude-lsp-enable))

(use-package lua-mode
  :ensure t
  :custom
  (lua-indent-level 2)
  ;; Don't align block contents to opening paren position
  (lua-indent-nested-block-content-align nil)
  (lua-indent-close-paren-align nil)
  ;; Indent contents of multi-line strings
  (lua-indent-string-contents t)
  :bind (:map lua-mode-map
              ("C-c C-b" . lua-send-buffer)
              ("C-c C-f" . lua-send-defun)
              ("C-c C-r" . lua-send-region)
              ("C-c C-z" . lua-show-process-buffer))
  :hook (lua-mode . (lambda ()
                      (run-hooks 'prelude-lua-mode-hook))))

(setq prelude-lua-mode-hook 'prelude-lua-mode-defaults)

(provide 'prelude-lua)

;;; prelude-lua.el ends here
