;;; prelude-corfu.el --- Corfu in-buffer completion setup
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Corfu is a minimal in-buffer completion UI -- a modern, lightweight
;; alternative to Company.  It pairs naturally with the
;; vertico/orderless/marginalia stack in `prelude-vertico'.
;;
;; Cape extends `completion-at-point-functions' with useful sources
;; (file paths, dabbrev, keywords, etc.).  This module enables a few
;; sensible defaults; add more from your personal config if you want.
;;
;; This module is mutually exclusive with `prelude-company' -- enable
;; one or the other, not both.

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

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.2)
  :init
  (global-corfu-mode)
  :config
  (corfu-popupinfo-mode)
  ;; sort candidates by recency of selection, so the ones you pick
  ;; often bubble to the top; the history persists across sessions
  ;; when savehist-mode is on (which Prelude enables in core)
  (corfu-history-mode))

(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(provide 'prelude-corfu)
;;; prelude-corfu.el ends here
