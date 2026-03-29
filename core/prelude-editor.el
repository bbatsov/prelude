;;; prelude-editor.el --- Emacs Prelude: enhanced core editing experience.
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Refinements of the core editing experience in Emacs.

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

;; Death to the tabs!  However, tabs historically indent to the next
;; 8-character offset; specifying anything else will cause *mass*
;; confusion, as it will change the appearance of every existing file.
;; In some cases (python), even worse -- it will change the semantics
;; (meaning) of the program.
;;
;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; hippie expand is dabbrev expand on steroids
(when prelude-hippie-expand
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-all-abbrevs
                                           try-expand-list
                                           try-expand-line
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol)))

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; smart pairing for all
(require 'smartparens-config)
(setq sp-base-key-bindings 'sp)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-smartparens-bindings)

;; On macOS, add Super-based alternatives for common structural
;; editing commands (à la Magnar Sveen's config)
(when (eq system-type 'darwin)
  (define-key smartparens-mode-map (kbd "s-s") #'sp-splice-sexp)
  (define-key smartparens-mode-map (kbd "s-<right>") #'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "s-<left>") #'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "s-<up>") #'sp-splice-sexp-killing-backward)
  (define-key smartparens-mode-map (kbd "s-<down>") #'sp-splice-sexp-killing-forward))

(show-smartparens-global-mode +1)

(define-key prog-mode-map (kbd "M-(") (prelude-wrap-with "("))
(define-key prog-mode-map (kbd "M-\"") (prelude-wrap-with "\""))

;; disable annoying blink-matching-paren
(setq blink-matching-paren nil)

;; diminish keeps the modeline tidy
(require 'diminish)

;; meaningful names for buffers with the same name
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" prelude-savefile-dir))
;; activate it for all buffers
(save-place-mode 1)

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" prelude-savefile-dir))
(savehist-mode +1)

;; save recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" prelude-savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)

(defun prelude-recentf-exclude-p (file)
  "A predicate to decide whether to exclude FILE from recentf."
  (let ((file-dir (file-truename (file-name-directory file))))
    (cl-some (lambda (dir)
               (string-prefix-p dir file-dir))
             (mapcar 'file-truename (list prelude-savefile-dir package-user-dir)))))

(add-to-list 'recentf-exclude 'prelude-recentf-exclude-p)

(recentf-mode +1)

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(require 'super-save)
;; add integration with ace-window
(add-to-list 'super-save-triggers 'ace-window)
(super-save-mode +1)
(diminish 'super-save-mode)

(define-advice set-buffer-major-mode (:after (buffer) prelude-set-major-mode)
  "Set buffer major mode according to `auto-mode-alist'."
  (let* ((name (buffer-name buffer))
         (mode (assoc-default name auto-mode-alist 'string-match)))
    (when (and mode (consp mode))
      (setq mode (car mode)))
    (with-current-buffer buffer (if mode (funcall mode)))))

;; highlight the current line
(global-hl-line-mode +1)

(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;; note - this should be after volatile-highlights is required
;; add the ability to cut the current line, without marking it
(require 'rect)
(require 'crux)
(crux-with-region-or-line kill-region)

;; tramp, for sudo access
(use-package tramp
  :defer t
  :init
  (setq tramp-default-method "ssh"))

(set-default 'imenu-auto-rescan t)

;; flyspell-mode does spell-checking on the fly as you type
(use-package flyspell
  :defer t
  :init
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra")))

(defun prelude-enable-flyspell ()
  "Enable command `flyspell-mode' if `prelude-flyspell' is not nil."
  (when (and prelude-flyspell (executable-find ispell-program-name))
    (flyspell-mode +1)))

(defun prelude-cleanup-maybe ()
  "Invoke `whitespace-cleanup' if `prelude-clean-whitespace-on-save' is not nil."
  (when prelude-clean-whitespace-on-save
    (whitespace-cleanup)))

(defun prelude-enable-whitespace ()
  "Enable `whitespace-mode' if `prelude-whitespace' is not nil."
  (when prelude-whitespace
    ;; keep the whitespace decent all the time (in this buffer)
    (add-hook 'before-save-hook 'prelude-cleanup-maybe nil t)
    (whitespace-mode +1)))

(add-hook 'text-mode-hook 'prelude-enable-flyspell)
(add-hook 'text-mode-hook 'prelude-enable-whitespace)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

(use-package expand-region
  :defer t)

;; bookmarks
(use-package bookmark
  :defer t
  :init
  (setq bookmark-default-file (expand-file-name "bookmarks" prelude-savefile-dir)
        bookmark-save-flag 1))

;; projectile is a project management mode
(when prelude-projectile
  (require 'projectile)
  (setq projectile-cache-file (expand-file-name  "projectile.cache" prelude-savefile-dir))
  (projectile-mode t))

;; avy allows us to effectively navigate to visible things
(use-package avy
  :defer t
  :init
  (setq avy-background t
        avy-style 'at-full))

;; show match count during isearch and query-replace
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

;; enable some really cool extensions like C-x C-j(dired-jump)
(use-package dired-x
  :after dired)

;; ediff - don't start another frame
(use-package ediff
  :defer t
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;; clean up obsolete buffers automatically
(require 'midnight)

;; smarter kill-ring navigation
(use-package browse-kill-ring
  :bind (("M-y" . browse-kill-ring)
         ("s-y" . browse-kill-ring)))

(define-advice exchange-point-and-mark (:before (&rest _) prelude-deactivate-mark)
  "When called with no active region, do not activate mark."
  (interactive
   (list (not (region-active-p)))))

(require 'tabify)
(defmacro with-region-or-buffer (func)
  "When called with no active region, call FUNC on current buffer."
  `(define-advice ,func (:before (&rest _) prelude-region-or-buffer)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point-min) (point-max))))))

(with-region-or-buffer indent-region)
(with-region-or-buffer untabify)

;; automatically indenting yanked text if in programming-modes
(defun prelude-yank-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) prelude-yank-indent-threshold)
      (indent-region beg end nil)))

(defun prelude-yank-indent-advice (&rest _args)
  "Indent yanked text if in a programming mode.
Does not indent if the mode is in `prelude-indent-sensitive-modes'."
  (when (and (not (member major-mode prelude-indent-sensitive-modes))
             (or (derived-mode-p 'prog-mode)
                 (member major-mode prelude-yank-indent-modes)))
    (let ((transient-mark-mode nil))
      (prelude-yank-indent-function (region-beginning) (region-end)))))

(advice-add 'yank :after #'prelude-yank-indent-advice)
(advice-add 'yank-pop :after #'prelude-yank-indent-advice)

;; abbrev config
(add-hook 'text-mode-hook 'abbrev-mode)
(diminish 'abbrev-mode)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;; whitespace-mode config
(use-package whitespace
  :defer t
  :init
  (setq whitespace-line-column 80
        whitespace-style '(face tabs empty trailing lines-tail)))

;; saner regex syntax
(use-package re-builder
  :defer t
  :init
  (setq reb-re-syntax 'string))

(use-package eshell
  :defer t
  :init
  (setq eshell-directory-name (expand-file-name "eshell" prelude-savefile-dir)))

(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" prelude-savefile-dir))

;; Compilation from Emacs
(use-package compile
  :defer t
  :hook (compilation-filter . ansi-color-compilation-filter)
  :init
  (setq compilation-ask-about-save nil
        compilation-always-kill t
        compilation-scroll-output 'first-error))

;; enable Prelude's keybindings
(prelude-mode t)

(defun prelude-maybe-enable-undo-tree ()
  "Enable `undo-tree' if `prelude-undo-tree' is not nil."
  (when prelude-undo-tree
    ;; supercharge your undo/redo with undo-tree
    (require 'undo-tree)
    ;; autosave the undo-tree history
    (setq undo-tree-history-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq undo-tree-auto-save-history t)
    (global-undo-tree-mode)
    (diminish 'undo-tree-mode)))


(prelude-maybe-enable-undo-tree)

;; enable winner-mode to manage window configurations
(winner-mode +1)

;; diff-hl
(global-diff-hl-mode +1)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; easy-kill
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

;; operate-on-number
(require 'operate-on-number)
(require 'smartrep)

(smartrep-define-key global-map "C-c ."
  '(("+" . apply-operation-to-number-at-point)
    ("-" . apply-operation-to-number-at-point)
    ("*" . apply-operation-to-number-at-point)
    ("/" . apply-operation-to-number-at-point)
    ("\\" . apply-operation-to-number-at-point)
    ("^" . apply-operation-to-number-at-point)
    ("<" . apply-operation-to-number-at-point)
    (">" . apply-operation-to-number-at-point)
    ("#" . apply-operation-to-number-at-point)
    ("%" . apply-operation-to-number-at-point)
    ("'" . operate-on-number-at-point)))

(defun prelude-server-visit-files-parse-numbers (args)
  "Parse line numbers from filenames for emacsclient.
Most console-based utilities print filenames in the format
'filename:linenumber'.  So you may wish to open filename in
that format.  Just call:

  emacsclient filename:linenumber

and file 'filename' will be opened and cursor set on line
'linenumber'."
  (list
   (mapcar (lambda (fn)
             (let ((name (car fn)))
               (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
                   (cons
                    (match-string 1 name)
                    (cons (string-to-number (match-string 2 name))
                          (string-to-number (or (match-string 3 name) ""))))
                 fn)))
           (car args))
   (cadr args)
   (caddr args)))

(advice-add 'server-visit-files :filter-args #'prelude-server-visit-files-parse-numbers)

;; use settings from .editorconfig file when present
(require 'editorconfig)
(editorconfig-mode 1)
(diminish 'editorconfig-mode)

(provide 'prelude-editor)

;;; prelude-editor.el ends here
