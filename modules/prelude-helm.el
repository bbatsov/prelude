;;; prelude-helm.el --- Helm setup
;;
;; Copyright © 2011-2014 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some config for Helm that follows thiks guide: http://tuhdo.github.io/helm-intro.html

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

(prelude-require-packages '(helm helm-projectile helm-descbinds))

(require 'helm)

;; must set before helm-config,  otherwise helm use default
;; prefix "C-x c", which is inconvenient because you can
;; accidentially pressed "C-x C-c"
(setq helm-command-prefix-key "C-c h")

(require 'helm-config)
(require 'helm-eshell)
(require 'helm-files)
(require 'helm-grep)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

(setq
 helm-google-suggest-use-curl-p t
 helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
 helm-quick-update t ; do not display invisible candidates
 helm-idle-delay 0.01 ; be idle for this many seconds, before updating in delayed sources.
 helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer
 helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
 helm-split-window-default-side 'other ;; open helm buffer in another window
 helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
 helm-buffers-favorite-modes (append helm-buffers-favorite-modes
                                     '(picture-mode artist-mode))
 helm-candidate-number-limit 500 ; limit the number of displayed canidates
 helm-M-x-requires-pattern 0    ; show all candidates when set to 0
 helm-ff-file-name-history-use-recentf t
 helm-move-to-line-cycle-in-source t ; move to end or beginning of source
                                        ; when reaching top or bottom of source.
 ido-use-virtual-buffers t             ; Needed in helm-buffers-list
 helm-buffers-fuzzy-matching t          ; fuzzy matching buffer names when non-nil
                                        ; useful in helm-mini that lists buffers
 )

(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h g") 'helm-do-grep)
(global-set-key (kbd "C-c h C-c w") 'helm-wikipedia-suggest)
(global-set-key (kbd "C-c h x") 'helm-register)
(global-set-key (kbd "C-c h SPC") 'helm-all-mark-rings)

;;; Save current position to mark ring
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

(defvar prelude-global-helm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-x") 'helm-M-x)
    (define-key map (kbd "M-y") 'helm-show-kill-ring)
    (define-key map (kbd "C-x b") 'helm-mini)
    (define-key map (kbd "C-x C-f") 'helm-find-files)
    (define-key map (kbd "C-h C-f") 'helm-apropos)
    (define-key map (kbd "C-h r") 'helm-info-emacs)
    (define-key map (kbd "C-h C-l") 'helm-locate-library)
    map)
  "Keymap for Helm to replace standard Prelude's commands")

(define-minor-mode prelude-global-helm-minor-mode
  "Minor mode to replace Prelude default commands with \\{prelude-global-helm-map}"
  :keymap prelude-global-helm-mode-map
  (progn
    ;; show minibuffer history with Helm
    (define-key minibuffer-local-map (kbd "M-l") 'helm-minibuffer-history)
    (define-key global-map [remap find-tag] 'helm-etags-select)
    (define-key global-map [remap list-buffers] 'helm-mini)

    ;; shell history.
    (define-key shell-mode-map (kbd "M-l") 'helm-comint-input-ring)

    ;; use helm to list eshell history
    (add-hook 'eshell-mode-hook
              #'(lambda ()
                  (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))))

(define-globalized-minor-mode prelude-global-helm-global-mode prelude-global-helm-minor-mode prelude-global-helm-global-mode-on)

(defun prelude-global-helm-global-mode-on ()
  "Turn on `prelude-global-helm-minor-mode'"
  (prelude-global-helm-minor-mode +1)
  )

(defun prelude-global-helm-global-mode-off ()
  "Turn off `prelude-global-helm-minor-mode'"
  (prelude-global-helm-minor-mode -1))

(helm-mode 1)

;; PACKAGE: helm-projectile

(require 'helm-projectile)
(setq projectile-completion-system 'helm)

(push "Press <C-c p h> to navigate a project in Helm." prelude-tips)

;; PACKAGE: helm-descbinds
(require 'helm-descbinds)
(helm-descbinds-mode)

(provide 'prelude-helm)

;;; prelude-helm.el ends here
