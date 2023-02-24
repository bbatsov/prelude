;;; prelude-lua.el --- Emacs Prelude: Lua programming configuration.
;;
;; Copyright © 2011-2023 Bozhidar Batsov
;;
;; Author: Xiongfei Shi <xiongfei.shi@icloud.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Basic configuration for Lua programming.

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
(prelude-require-packages '(lua-mode))

(with-eval-after-load 'lua-mode
  (setq lua-indent-level 2)
  (setq lua-indent-nested-block-content-align nil)
  (setq lua-indent-close-paren-align nil)
  (setq lua-indent-string-contents t)

  (define-key lua-mode-map (kbd "C-c C-b") 'lua-send-buffer)
  (define-key lua-mode-map (kbd "C-c C-l") 'lua-send-current-line)
  (define-key lua-mode-map (kbd "C-c C-f") 'lua-send-defun)
  (define-key lua-mode-map (kbd "C-c C-r") 'lua-send-region)
  (define-key lua-mode-map (kbd "C-c C-z") 'lua-show-process-buffer))

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(provide 'prelude-lua)

;;; prelude-lua ends here
