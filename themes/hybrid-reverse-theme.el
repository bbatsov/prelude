;;; hybrid-reverse-theme.el --- Emacs theme with material color scheme -*- lexical-binding: t; -*-

;; Author: Riyyi
;; URL: https://github.com/riyyi/emacs-hybrid-reverse
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: faces, theme

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Material color scheme based on Hybrid Reverse for Vim.
;;
;; Supported packages:
;; - ace-window
;; - auto-complete
;; - avy
;; - centaur-tabs
;; - column-enforce-mode
;; - company-mode
;; - dashboard
;; - deft
;; - doom-modeline
;; - elfeed
;; - evil
;; - flycheck
;; - hydra
;; - ido-vertical-mode
;; - linum-relative
;; - lsp-ui
;; - magit
;; - mu4e
;; - neotree
;; - org-roam
;; - paradox
;; - php-mode
;; - popup
;; - powerline
;; - powerline-evil
;; - rainbow-delimiters
;; - selectrum
;; - smart-mode-line
;; - spaceline
;; - switch-window
;; - telephone-line
;; - transient
;; - treemacs
;; - vterm
;; - which-key
;; - winum
;;
;; Supported defaults:
;; - custom (M-x customize)
;; - dired
;; - display-line-numbers-mode
;; - font-lock-mode
;; - ido-mode
;; - info
;; - isearch
;; - linum-mode
;; - man
;; - message-mode
;; - mode-line
;; - org-mode*
;; - outline-mode
;; - ruler-mode
;; - shell-script-mode
;; - show-paren-mode
;; - tab-bar-mode
;; - tab-line-mode
;; - term
;; - tty-menu
;; - whitespace-mode

;;; References:

;; Awesome Emacs
;;   https://github.com/emacs-tw/awesome-emacs
;; GNU Emacs
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Creating-Custom-Themes.html
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Faces.html
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Standard-Faces.html
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Face-Customization.html
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Feature-Testing.html

;;; Code:

(when (version< emacs-version "24.1")
  (error "Hybrid Reverse theme requires Emacs 24.1 or later!"))

(deftheme hybrid-reverse "Face colors like Tomorrow Night with a dark background.")

(defgroup hybrid-reverse nil
  "Faces used by the Hybrid Reverse theme."
  :group 'faces
  :prefix "hybrid-reverse-"
  :link '(url-link :tag "GitHub" "https://github.com/riyyi/emacs-hybrid-reverse")
  :tag "Hybrid Reverse")

;;; User customize options

(eval-and-compile
  (defcustom hybrid-reverse-theme-override-colors-alist '()
	"Association list of palette color overrides.
Values can be mapped to variables, using the same syntax as the
one present in `hybrid-reverse-theme-default-colors-alist'."
	:group 'hybrid-reverse
	:type '(alist
			:key-type (string :tag "Name")
			:value-type (string :tag " Hex"))))

;;; Color Palette

(eval-and-compile
  (defconst hybrid-reverse-theme-default-colors-alist
	'(
	  ("hr-white"      . "#ffffff") ; grey100
	  ("hr-white+1"    . "#e4e4e4") ; grey90~
	  ("_hr-white+2"   . "#d0d0d0") ; grey82~
	  ("_hr-white+3"   . "#cccccc") ; grey80
	  ("hr-fg"         . "#c5c8c6") ; grey78~
	  ("_hr-white+4"   . "#bcbcbc") ; grey74~
	  ("hr-white+5"    . "#9e9e9e") ; grey62
	  ("hr-white+6"    . "#707880") ; grey46~
	  ;; ----------------------
	  ("hr-black-8"    . "#656565") ; grey40~
	  ("hr-black-7"    . "#5f5f5f") ; grey37~
	  ("hr-black-6"    . "#373b41") ; grey24~
	  ("hr-black-5"    . "#383838") ; grey22
	  ("_hr-black-4"   . "#303030") ; grey19
	  ("hr-black-3"    . "#282a2e") ; grey16~
	  ("hr-black-2"    . "#212121") ; grey13 ; Added
	  ("_hr-black-1"   . "#1d1f21") ; grey12
	  ("hr-bg"         . "#1c1c1c") ; grey11
	  ("hr-black"      . "#000000") ; grey0
	  ;; ----------------------
	  ("_hr-red-1"     . "#ffd7d7")
	  ("hr-red"        . "#cc6666")
	  ("hr-red+1"      . "#a54242") ; Added
	  ("_hr-red+2"     . "#5f0000")
	  ;; ----------------------
	  ("hr-orange"     . "#de935f")
	  ("_hr-orange+1"  . "#875f00")
	  ;; ----------------------
	  ("hr-yellow"     . "#f0c674")
	  ("_hr-yellow+2"  . "#5f5f00")
	  ;; ----------------------
	  ("_hr-green-1"   . "#d7ffd7")
	  ("hr-green"      . "#b5bd68")
	  ("hr-green+1"    . "#8c9440") ; Added
	  ("hr-green+2"    . "#5f875f")
	  ("_hr-green+3"   . "#005f00")
	  ;; ----------------------
	  ("hr-cyan"       . "#8abeb7")
	  ("hr-cyan+1"     . "#5e8d87") ; Added
	  ("_hr-cyan+2"    . "#005f5f")
	  ;; ----------------------
	  ("hr-blue-1"     . "#d7d7ff")
	  ("hr-blue"       . "#81a2be")
	  ("hr-blue+1"     . "#5f819d") ; Added
	  ("hr-blue+2"     . "#5f5f87")
	  ("_hr-blue+3"    . "#00005f")
	  ;; ----------------------
	  ("hr-magenta"    . "#b294bb")
	  ("hr-magenta+1"  . "#85678f") ; Added
	  ("_hr-magenta+2" . "#5f005f"))
	"The entire color palette of Hybrid Reverse theme.
Each element has the form (NAME . HEX).")

  (defmacro hybrid-reverse-theme-with-color-variables (&rest body)
	"`let' bind all colors around BODY.
Also bind `class' to ((class color) (min-colors 89))."
	(declare (indent 0))
	`(let ((class '((class color) (min-colors 89)))
           ,@(mapcar (lambda (cons)
                       (list (intern (car cons)) (cdr cons)))
					 (append hybrid-reverse-theme-default-colors-alist
							 hybrid-reverse-theme-override-colors-alist)))
       ,@body)))

;;; Theme Faces

(hybrid-reverse-theme-with-color-variables
  (custom-theme-set-faces
   'hybrid-reverse

;;;; --- Built-in ---

;;;;; Basic coloring
   `(cursor                                   ((,class                           :background ,hr-white+1)))
   `(default                                  ((,class :foreground ,hr-fg        :background ,hr-bg)))
   `(error                                    ((,class :foreground ,hr-red       :background ,hr-bg        :weight bold)))
   `(escape-glyph                             ((,class :foreground ,hr-cyan)))
   `(homoglyph                                ((,class :foreground ,hr-cyan)))
   `(nobreak-hyphen                           ((,class :foreground ,hr-cyan)))
   `(nobreak-space                            ((,class :foreground ,hr-cyan                                :underline t)))
   `(success                                  ((,class :foreground ,hr-green                               :weight bold)))
   `(warning                                  ((,class :foreground ,hr-orange                              :weight bold)))

;;;;; UI
   `(border                                   ((,class :foreground ,hr-white+5)))
   `(fill-column-indicator                    ((,class :foreground ,hr-black-3)))
   `(fringe                                   ((,class :foreground ,hr-fg        :background ,hr-black-2)))
   `(highlight                                ((,class :foreground ,hr-yellow    :background ,hr-bg)))
   `(highlight-changes                        ((,class :foreground ,hr-red       :background ,hr-bg)))
   `(hl-line                                  ((,class                           :background ,hr-black-3   :extend t)))
   `(link                                     ((,class :foreground ,hr-blue      :background ,hr-bg)))
   `(link-visited                             ((,class :foreground ,hr-blue      :background ,hr-bg)))
   `(minibuffer-prompt                        ((,class :foreground ,hr-blue      :background ,hr-bg)))
   `(region                                   ((,class                           :background ,hr-black-6)))
   `(secondary-selection                      ((,class                           :background ,hr-black-3)))
   `(tooltip                                  ((,class                           :background ,hr-black-3)))
   `(tool-bar                                 ((,class                           :background ,hr-black-3)))
   `(vertical-border                          ((,class :foreground ,hr-white+5)))
   `(widget-button                            ((,class                                                     :weight normal)))
   `(widget-button-pressed                    ((,class :foreground ,hr-orange)))
   `(widget-field                             ((,class :background ,hr-black-6)))
   `(window-divider                           ((,class :foreground ,hr-white+5)))
   `(window-divider-first-pixel               ((,class :foreground ,hr-black-6)))
   `(window-divider-last-pixel                ((,class :foreground ,hr-black-6)))

;;;;; custom (M-x customize
   `(custom-button                            ((,class :foreground ,hr-fg        :background ,hr-black-3   :box (:line-width 2 :color ,hr-black-8))))
   `(custom-button-mouse                      ((,class                           :background ,hr-black-6   :inherit custom-button)))
   `(custom-button-pressed                    ((,class                           :background ,hr-black-8   :inherit custom-button)))
   `(custom-button-pressed-unraised           ((,class :foreground ,hr-magenta                             :inherit custom-button-unraised)))
   `(custom-button-unraised                   ((,class                                                     :underline t)))
   `(custom-changed                           ((,class :foreground ,hr-bg        :background ,hr-blue)))
   `(custom-comment                           ((,class :background ,hr-black-6)))
   `(custom-comment-tag                       ((,class :foreground ,hr-fg)))
   `(custom-documentation                     ((,class :foreground ,hr-fg)))
   `(custom-face-tag                          ((,class                                                     :inherit custom-variable-tag)))
   `(custom-group-subtitle                    ((,class :foreground ,hr-orange                              :weight bold)))
   `(custom-group-tag                         ((,class :foreground ,hr-orange                              :weight bold)))
   `(custom-group-tag-1                       ((,class :foreground ,hr-magenta                             :inherit variable-pitch :weight bold :height 1.2)))
   `(custom-invalid                           ((,class                                                     :inherit error)))
   `(custom-link                              ((,class                                                     :inherit link :underline t)))
   `(custom-modified                          ((,class :foreground ,hr-bg        :background ,hr-blue)))
   `(custom-rogue                             ((,class :foreground ,hr-orange)))
   `(custom-saved                             ((,class                                                     :underline t)))
   `(custom-set                               ((,class :foreground ,hr-bg        :background ,hr-fg)))
   `(custom-state                             ((,class :foreground ,hr-green)))
   `(custom-themed                            ((,class :foreground ,hr-bg        :background ,hr-blue)))
   `(custom-variable-button                   ((,class                                                     :weight bold :underline t)))
   `(custom-variable-tag                      ((,class :foreground ,hr-blue)))
   `(custom-visibility                        ((,class                                                     :inherit link :underline t :height 0.8)))

;;;;; dired
   `(dired-directory                          ((,class :foreground ,hr-magenta)))
   `(dired-flagged                            ((,class                                                     :inherit error)))
   `(dired-header                             ((,class :foreground ,hr-orange)))
   `(dired-ignored                            ((,class                                                     :inherit font-lock-comment-face)))
   `(dired-mark                               ((,class :foreground ,hr-blue)))
   `(dired-marked                             ((,class                                                     :inherit warning)))
   `(dired-perm-write                         ((,class                                                     :inherit font-lock-comment-delimiter-face)))
   `(dired-symlink                            ((,class :foreground ,hr-cyan)))
   `(dired-warning                            ((,class                                                     :inherit warning)))

;;;;; font-lock-mode
   `(font-lock-builtin-face                   ((,class :foreground ,hr-blue)))
   `(font-lock-comment-face                   ((,class :foreground ,hr-white+6                             :slant italic)))
   `(font-lock-comment-delimiter-face         ((,class :foreground ,hr-white+6                             :slant italic)))
   `(font-lock-constant-face                  ((,class :foreground ,hr-fg)))
   `(font-lock-doc-face                       ((,class :foreground ,hr-white+6)))
   `(font-lock-function-name-face             ((,class :foreground ,hr-fg)))
   `(font-lock-keyword-face                   ((,class :foreground ,hr-blue)))
   `(font-lock-negation-char-face             ((,class :foreground ,hr-cyan)))
   `(font-lock-preprocessor-face              ((,class :foreground ,hr-cyan)))
   `(font-lock-regexp-grouping-backslash      ((,class :foreground ,hr-yellow)))
   `(font-lock-regexp-grouping-construct      ((,class :foreground ,hr-green)))
   `(font-lock-string-face                    ((,class :foreground ,hr-green)))
   `(font-lock-type-face                      ((,class :foreground ,hr-orange)))
   `(font-lock-variable-name-face             ((,class :foreground ,hr-red)))
   `(font-lock-warning-face                   ((,class :foreground ,hr-red                                 :weight bold)))

;;;;; ido-mode
   `(ido-first-match                          ((,class :foreground ,hr-orange)))
   `(ido-indicator                            ((,class :foreground ,hr-red)))
   `(ido-only-match                           ((,class :foreground ,hr-green)))
   `(ido-subdir                               ((,class :foreground ,hr-magenta)))

;;;;; info
   ;; `(Info-quoted                              ((,class                                                     :inherit fixed-pitch-serif)))
   `(info-header-node                         ((,class                                                     :inherit info-node)))
   `(info-header-xref                         ((,class :foreground ,hr-blue :weight bold)))
   `(info-index-match                         ((,class                                                     :inherit match)))
   `(info-menu-header                         ((,class                                                     :inherit variable-pitch :weight bold :height 1.1)))
   `(info-menu-star                           ((,class :foreground ,hr-fg)))
   `(info-node                                ((,class                                                     :weight bold :slant italic)))
   `(info-title-1                             ((,class                                                     :inherit info-title-2 :height 1.2)))
   `(info-title-2                             ((,class                                                     :inherit info-title-3 :height 1.2)))
   `(info-title-3                             ((,class                                                     :inherit info-title-4 :height 1.2)))
   `(info-title-4                             ((,class :foreground ,hr-orange                              :inherit variable-pitch :weight bold)))
   `(info-xref                                ((,class                                                     :inherit link)))
   `(info-xref-visited                        ((,class                                                     :inherit (link-visited info-xref))))

;;;;; search
   `(isearch                                  ((,class :foreground ,hr-bg        :background ,hr-yellow)))
   `(isearch-fail                             ((,class :foreground ,hr-fg        :background ,hr-red+1)))
   `(lazy-highlight                           ((,class :foreground ,hr-bg        :background ,hr-white+5)))
   `(match                                    ((,class :foreground ,hr-bg        :background ,hr-blue)))

;;;;; linum-mode
   `(linum                                    ((,class :foreground ,hr-white+5   :background ,hr-black-2)))

;;;;; display-line-numbers-mode (Emacs >=26.1
   `(line-number                              ((,class :foreground ,hr-white+5   :background ,hr-black-2)))
   `(line-number-current-line                 ((,class :foreground ,hr-yellow    :background ,hr-black-2   :weight bold)))

;;;;; man
   `(Man-overstrike                           ((,class :foreground ,hr-blue                                :weight bold)))
   `(Man-reverse                              ((,class :foreground ,hr-bg        :background ,hr-fg        :weight bold)))
   `(Man-underline                            ((,class :foreground ,hr-magenta                             :underline t)))

;;; message-mode
   `(message-cited-text                       ((,class :foreground ,hr-white+5)))
   `(message-header-cc                        ((,class :foreground ,hr-red)))
   `(message-header-name                      ((,class :foreground ,hr-blue                                :weight bold)))
   `(message-header-newsgroups                ((,class :foreground ,hr-cyan)))
   `(message-header-other                     ((,class :foreground ,hr-green)))
   `(message-header-subject                   ((,class :foreground ,hr-green)))
   `(message-header-to                        ((,class :foreground ,hr-red)))
   `(message-header-xheader                   ((,class :foreground ,hr-yellow)))
   `(message-mml                              ((,class :foreground ,hr-yellow)))
   `(message-separator                        ((,class                                                     :inherit font-lock-comment-face)))

;;;;; mode-line
   `(header-line                              ((,class                           :background ,hr-black-3)))
   `(mode-line                                ((,class :foreground ,hr-fg        :background ,hr-black-3)))
   `(mode-line-buffer-id                      ((,class                                                     :weight bold)))
   `(mode-line-emphasis                       ((,class                                                     :weight bold)))
   `(mode-line-highlight                      ((,class :foreground ,hr-yellow)))
   `(mode-line-inactive                       ((,class :foreground ,hr-black-8   :background ,hr-black-2)))

;;;;; org-mode
   `(org-agenda-structure                     ((,class :foreground ,hr-magenta)))
   `(org-agenda-current-time                  ((,class :foreground ,hr-yellow)))
   `(org-agenda-date                          ((,class :foreground ,hr-blue                                :underline nil)))
   `(org-agenda-done                          ((,class :foreground ,hr-green)))
   `(org-agenda-dimmed-todo-face              ((,class :foreground ,hr-white+6)))
   `(org-block                                ((,class                           :background ,hr-black-3   :extend t)))
   `(org-block-begin-line                     ((,class :foreground ,hr-white+6   :background ,hr-bg        :slant italic)))
   `(org-block-end-line                       ((,class                                                     :inherit org-block-begin-line)))
   `(org-code                                 ((,class :foreground ,hr-yellow)))
   `(org-column                               ((,class                           :background ,hr-black-6)))
   `(org-column-title                         ((,class                                                     :inherit org-column :weight bold :underline t)))
   `(org-date                                 ((,class :foreground ,hr-blue                                :underline t)))
   `(org-date-selected                        ((,class :foreground ,hr-cyan                                :inverse-video t)))
   `(org-document-info                        ((,class :foreground ,hr-green)))
   `(org-document-info-keyword                ((,class :foreground ,hr-blue)))
   `(org-document-title                       ((,class :foreground ,hr-green                               :weight bold)))
   `(org-done                                 ((,class :foreground ,hr-green)))
   `(org-drawer                               ((,class :foreground ,hr-blue)))
   `(org-ellipsis                             ((,class :foreground ,hr-white+6)))
   `(org-footnote                             ((,class :foreground ,hr-cyan)))
   `(org-formula                              ((,class :foreground ,hr-red)))
   `(org-hide                                 ((,class :foreground ,hr-bg        :background ,hr-bg)))
   `(org-habit-alert-face                     ((,class :foreground ,hr-bg        :background ,hr-yellow)))
   `(org-habit-alert-future-face              ((,class :foreground ,hr-bg        :background ,hr-orange)))
   `(org-habit-clear-face                     ((,class :foreground ,hr-bg        :background ,hr-white+6)))
   `(org-habit-clear-future-face              ((,class :foreground ,hr-bg        :background ,hr-magenta)))
   `(org-habit-overdue-face                   ((,class :foreground ,hr-bg        :background ,hr-blue)))
   `(org-habit-overdue-future-face            ((,class :foreground ,hr-bg        :background ,hr-red)))
   `(org-habit-ready-face                     ((,class :foreground ,hr-bg        :background ,hr-cyan)))
   `(org-habit-ready-future-face              ((,class :foreground ,hr-bg        :background ,hr-green)))
   `(org-link                                 ((,class :foreground ,hr-blue                                :underline t)))
   `(org-mode-line-clock-overrun              ((,class                           :background ,hr-red       :inherit mode-line)))
   `(org-scheduled                            ((,class :foreground ,hr-green)))
   `(org-scheduled-previously                 ((,class :foreground ,hr-cyan)))
   `(org-scheduled-today                      ((,class :foreground ,hr-green)))
   `(org-special-keyword                      ((,class :foreground ,hr-orange)))
   `(org-table                                ((,class :foreground ,hr-magenta)))
   `(org-tag                                  ((,class :foreground ,hr-red                                 :weight bold)))
   `(org-time-grid                            ((,class :foreground ,hr-yellow)))
   `(org-todo                                 ((,class :foreground ,hr-red)))
   `(org-upcoming-deadline                    ((,class :foreground ,hr-orange)))
   `(org-warning                              ((,class :foreground ,hr-red                                 :weight bold)))

;;;;; outline-mode
   `(outline-1                                ((,class :foreground ,hr-red)))
   `(outline-2                                ((,class :foreground ,hr-green)))
   `(outline-3                                ((,class :foreground ,hr-blue)))
   `(outline-4                                ((,class :foreground ,hr-yellow)))
   `(outline-5                                ((,class :foreground ,hr-red)))
   `(outline-6                                ((,class :foreground ,hr-green)))
   `(outline-7                                ((,class :foreground ,hr-blue)))
   `(outline-8                                ((,class :foreground ,hr-yellow)))

;;;;; ruler-mode
   `(ruler-mode-column-number                 ((,class :foreground ,hr-fg                                  :inherit ruler-mode-default :weight bold)))
   `(ruler-mode-comment-column                ((,class :foreground ,hr-magenta                             :inherit ruler-mode-default)))
   `(ruler-mode-current-column                ((,class :foreground ,hr-orange                              :inherit ruler-mode-default :weight bold)))
   `(ruler-mode-default                       ((,class :foreground ,hr-fg        :background ,hr-black-3)))
   `(ruler-mode-fill-column                   ((,class :foreground ,hr-red                                 :inherit ruler-mode-default)))
   `(ruler-mode-fringes                       ((,class :foreground ,hr-cyan                                :inherit ruler-mode-default)))
   `(ruler-mode-goal-column                   ((,class :foreground ,hr-blue                                :inherit ruler-mode-default)))
   `(ruler-mode-margins                       ((,class                           :background ,hr-bg        :inherit ruler-mode-default)))
   `(ruler-mode-pad                           ((,class :foreground ,hr-fg        :background ,hr-black-6)))
   `(ruler-mode-tab-stop                      ((,class :foreground ,hr-yellow                              :inherit ruler-mode-default )))

;;;;; shell-script-mode
   `(sh-heredoc                               ((,class :foreground nil                                     :inherit font-lock-string-face)))
   `(sh-quoted-exec                           ((,class :foreground nil                                     :inherit font-lock-preprocessor-face)))

;;;;; show-paren-mode
   `(show-paren-match                         ((,class :foreground ,hr-cyan      :background ,hr-blue+2    :weight bold)))
   `(show-paren-match-expression              ((,class                                                     :inherit show-paren-match)))
   `(show-paren-mismatch                      ((,class :foreground ,hr-white+1   :background ,hr-red       :weight bold)))

;;;;; tab-bar-mode
   `(tab-bar                                  ((,class                           :background ,hr-black-3)))
   `(tab-bar-tab                              ((,class :foreground ,hr-white     :background ,hr-black-6)))
   `(tab-bar-tab-inactive                     ((,class                                                     :inherit tab-bar)))

;;;;; tab-line-mode
   `(tab-line                                 ((,class                           :background ,hr-black-3)))
   `(tab-line-close-highlight                 ((,class :foreground ,hr-orange                              :weight bold :underline t)))
   `(tab-line-highlight                       ((,class :foreground ,hr-white     :background ,hr-black-6)))
   `(tab-line-tab                             ((,class                           :background ,hr-black-3)))
   `(tab-line-tab-current                     ((,class :foreground ,hr-white     :background ,hr-black-6)))
   `(tab-line-tab-inactive                    ((,class                                                     :inherit tab-line-tab)))

;;;;; term
   `(term                                     ((,class :foreground ,hr-fg        :background ,hr-bg)))
   `(term-bold                                ((,class                                                     :weight bold)))
   `(term-color-black                         ((,class :foreground ,hr-black-3   :background ,hr-black-6)))
   `(term-color-blue                          ((,class :foreground ,hr-blue+1    :background ,hr-blue)))
   `(term-color-cyan                          ((,class :foreground ,hr-cyan+1    :background ,hr-cyan)))
   `(term-color-green                         ((,class :foreground ,hr-green+1   :background ,hr-green)))
   `(term-color-magenta                       ((,class :foreground ,hr-magenta+1 :background ,hr-magenta)))
   `(term-color-red                           ((,class :foreground ,hr-red+1     :background ,hr-red)))
   `(term-color-white                         ((,class :foreground ,hr-white+6   :background ,hr-fg)))
   `(term-color-yellow                        ((,class :foreground ,hr-orange    :background ,hr-yellow)))
   `(term-underline                           ((,class                                                     :underline t)))

;;;;; tty-menu
   `(tty-menu-disabled-face                   ((,class                           :background ,hr-black-3)))
   `(tty-menu-enabled-face                    ((,class :foreground ,hr-white     :background ,hr-black-6   :weight bold)))
   `(tty-menu-selected-face                   ((,class                           :background ,hr-orange)))

;;;;; whitespace-mode
   `(trailing-whitespace                      ((,class :foreground ,hr-black-6   :background ,hr-orange)))
   `(whitespace-big-indent                    ((,class :foreground ,hr-black-6   :background ,hr-red)))
   `(whitespace-empty                         ((,class :foreground ,hr-black-6   :background ,hr-yellow)))
   `(whitespace-hspace                        ((,class :foreground ,hr-white+5   :background ,hr-black-6)))
   `(whitespace-indentation                   ((,class :foreground ,hr-white+5   :background ,hr-black-6)))
   `(whitespace-line                          ((,class :foreground ,hr-orange    :background ,hr-black-6)))
   `(whitespace-newline                       ((,class :foreground ,hr-white+5   :background ,hr-bg)))
   `(whitespace-space                         ((,class :foreground ,hr-white+5   :background ,hr-black-6)))
   `(whitespace-space-after-tab               ((,class :foreground ,hr-yellow    :background ,hr-black-6)))
   `(whitespace-space-before-tab              ((,class :foreground ,hr-orange    :background ,hr-black-6)))
   `(whitespace-tab                           ((,class :foreground ,hr-white+5   :background ,hr-black-6)))
   `(whitespace-trailing                      ((,class :foreground ,hr-black-6   :background ,hr-orange)))

;;;; --- Third-party ---

;;;;; ace-window
   `(aw-background-face                       ((,class                           :background ,hr-black-2)))
   `(aw-key-face                              ((,class :foreground ,hr-blue)))
   `(aw-leading-char-face                     ((,class :foreground ,hr-yellow                              :weight bold :height 3.0)))
   `(aw-minibuffer-leading-char-face          ((,class :foreground ,hr-yellow                              :weight bold)))
   `(aw-mode-line-face                        ((,class                                                     :inherit mode-line)))

;;;;; auto-complete
   `(ac-candidate-face                        ((,class                           :background ,hr-black-3)))
   `(ac-candidate-mouse-face                  ((,class :foreground ,hr-yellow    :background ,hr-black-6)))
   `(ac-completion-face                       ((,class :foreground ,hr-yellow    :background ,hr-black-3)))
   `(ac-gtags-candidate-face                  ((,class                           :background ,hr-black-3)))
   `(ac-gtags-selection-face                  ((,class :foreground ,hr-yellow    :background ,hr-black-6)))
   `(ac-selection-face                        ((,class :foreground ,hr-yellow    :background ,hr-black-6)))
   `(ac-yasnippet-candidate-face              ((,class                           :background ,hr-black-3)))
   `(ac-yasnippet-selection-face              ((,class :foreground ,hr-yellow    :background ,hr-black-6)))

;;;;; avy
   `(avy-background-face                      ((,class :foreground ,hr-bg        :background ,hr-yellow    :weight bold)))
   `(avy-lead-face                            ((,class :foreground ,hr-bg        :background ,hr-yellow    :weight bold)))
   `(avy-lead-face-0                          ((,class :foreground ,hr-bg        :background ,hr-yellow    :weight bold)))
   `(avy-lead-face-1                          ((,class :foreground ,hr-bg        :background ,hr-yellow    :weight bold)))
   `(avy-lead-face-2                          ((,class :foreground ,hr-bg        :background ,hr-yellow    :weight bold)))

;;;;; centaur-tabs
   `(centaur-tabs-active-bar-face             ((,class :foreground ,hr-bg        :background ,hr-cyan)))
   `(centaur-tabs-close-mouse-face            ((,class :foreground ,hr-orange                              :weight bold :underline t)))
   `(centaur-tabs-close-selected              ((,class                                                     :inherit centaur-tabs-selected)))
   `(centaur-tabs-close-unselected            ((,class                                                     :inherit centaur-tabs-unselected)))
   `(centaur-tabs-default                     ((,class :foreground ,hr-black-3   :background ,hr-black-3)))
   `(centaur-tabs-modified-marker-selected    ((,class :foreground ,hr-white     :background ,hr-black-6)))
   `(centaur-tabs-modified-marker-unselected  ((,class :foreground ,hr-fg        :background ,hr-black-3)))
   `(centaur-tabs-selected                    ((,class :foreground ,hr-white     :background ,hr-black-6)))
   `(centaur-tabs-selected-modified           ((,class :foreground ,hr-white     :background ,hr-black-6)))
   `(centaur-tabs-unselected                  ((,class :foreground ,hr-fg        :background ,hr-black-3)))
   `(centaur-tabs-unselected-modified         ((,class :foreground ,hr-fg        :background ,hr-black-3)))

;;;;; column-enforce-mode
   `(column-enforce-face                      ((,class :foreground ,hr-orange    :background ,hr-black-6)))

;;;;; company-mode
   `(company-preview                          ((,class                           :background ,hr-black-3)))
   `(company-preview-common                   ((,class :foreground ,hr-yellow    :background ,hr-black-3)))
   `(company-scrollbar-bg                     ((,class                           :background ,hr-black-3)))
   `(company-scrollbar-fg                     ((,class                           :background ,hr-black-6)))
   `(company-tooltip                          ((,class :foreground ,hr-fg        :background ,hr-black-3)))
   `(company-tooltip-annotation               ((,class :foreground ,hr-blue      :background ,hr-black-3)))
   `(company-tooltip-annotation-selection     ((,class :foreground ,hr-yellow    :background ,hr-black-6)))
   `(company-tooltip-common                   ((,class :foreground ,hr-bg        :background ,hr-white+5)))
   `(company-tooltip-common-selection         ((,class :foreground ,hr-bg        :background ,hr-yellow)))
   `(company-tooltip-mouse                    ((,class :foreground ,hr-yellow    :background ,hr-black-6)))
   `(company-tooltip-selection                ((,class :foreground ,hr-yellow    :background ,hr-black-6)))

;;;;; dashboard
   `(dashboard-banner-logo-title              ((,class :foreground ,hr-fg)))
   `(dashboard-footer                         ((,class :foreground ,hr-white+6)))
   `(dashboard-heading                        ((,class :foreground ,hr-blue)))
   `(dashboard-items-face                     ((,class                                                     :inherit widget-button)))
   `(dashboard-navigator                      ((,class :foreground ,hr-blue)))
   `(dashboard-no-items-face                  ((,class                                                     :inherit widget-button)))
   `(dashboard-text-banner                    ((,class :foreground ,hr-blue)))

;;;;; deft
   `(deft-filter-string-error-face            ((,class :inherit error)))
   `(deft-filter-string-face                  ((,class :foreground ,hr-green)))
   `(deft-header-face                         ((,class :foreground ,hr-orange                              :weight bold)))
   `(deft-separator-face                      ((,class                                                     :inherit font-lock-comment-delimiter-face)))
   `(deft-summary-face                        ((,class                                                     :inherit font-lock-comment-face)))
   `(deft-time-face                           ((,class :foreground ,hr-magenta)))
   `(deft-title-face                          ((,class :foreground ,hr-blue                                :weight bold)))

;;;;; doom-modeline
   `(doom-modeline-bar                        ((,class                           :background ,hr-blue)))
   `(doom-modeline-bar-inactive               ((,class                                                     :inherit mode-line-inactive)))
   `(doom-modeline-battery-charging           ((,class :foreground ,hr-fg)))
   `(doom-modeline-battery-critical           ((,class                                                     :inherit warning)))
   `(doom-modeline-battery-error              ((,class                                                     :inherit error)))
   `(doom-modeline-battery-full               ((,class :foreground ,hr-green)))
   `(doom-modeline-battery-normal             ((,class :foreground ,hr-fg)))
   `(doom-modeline-battery-warning            ((,class :foreground ,hr-orange)))
   `(doom-modeline-buffer-file                ((,class                                                     :inherit mode-line-emphasis)))
   `(doom-modeline-buffer-major-mode          ((,class :foreground ,hr-blue                                :inherit mode-line-emphasis)))
   `(doom-modeline-buffer-minor-mode          ((,class                           :background ,hr-cyan)))
   `(doom-modeline-buffer-modified            ((,class :foreground ,hr-orange                              :weight bold)))
   `(doom-modeline-buffer-path                ((,class                                                     :weight bold)))
   `(doom-modeline-buffer-timemachine         ((,class                                                     :inherit doom-modeline-buffer-file :underline t :slant italic)))
   `(doom-modeline-debug                      ((,class :foreground ,hr-orange)))
   `(doom-modeline-evil-emacs-state           ((,class :foreground ,hr-magenta)))
   `(doom-modeline-evil-insert-state          ((,class :foreground ,hr-green)))
   `(doom-modeline-evil-motion-state          ((,class :foreground ,hr-cyan)))
   `(doom-modeline-evil-normal-state          ((,class :foreground ,hr-blue)))
   `(doom-modeline-evil-operator-state        ((,class :foreground ,hr-black-8)))
   `(doom-modeline-evil-replace-state         ((,class :foreground ,hr-orange)))
   `(doom-modeline-evil-visual-state          ((,class :foreground ,hr-magenta)))
   `(doom-modeline-highlight                  ((,class :foreground ,hr-blue)))
   `(doom-modeline-host                       ((,class                                                     :slant italic)))
   `(doom-modeline-info                       ((,class :foreground ,hr-cyan                                :weight bold)))
   `(doom-modeline-input-method               ((,class                                                     :inherit mode-line-emphasis)))
   `(doom-modeline-input-method-alt           ((,class                                                     :inherit font-lock-doc-face :weight bold)))
   `(doom-modeline-lsp-error                  ((,class                                                     :inherit error :weight normal)))
   `(doom-modeline-lsp-running                ((,class :foreground ,hr-cyan)))
   `(doom-modeline-lsp-success                ((,class                                                     :inherit success :weight normal)))
   `(doom-modeline-lsp-warning                ((,class                                                     :inherit warning :weight normal)))
   `(doom-modeline-panel                      ((,class                                                     :inherit mode-line-highlight)))
   `(doom-modeline-persp-buffer-not-in-persp  ((,class                                                     :inherit font-lock-doc-face :weight bold :slant italic)))
   `(doom-modeline-persp-name                 ((,class                                                     :inherit font-lock-comment-face)))
   `(doom-modeline-project-dir                ((,class :foreground ,hr-magenta                             :weight bold)))
   `(doom-modeline-project-parent-dir         ((,class                                                     :inherit font-lock-comment-face :weight bold)))
   `(doom-modeline-project-root-dir           ((,class                                                     :inherit mode-line-emphasis)))
   `(doom-modeline-repl-success               ((,class :foreground ,hr-green)))
   `(doom-modeline-repl-warning               ((,class :foreground ,hr-orange)))
   `(doom-modeline-spc-face                   ((,class                                                     :inherit mode-line)))
   `(doom-modeline-unread-number              ((,class                                                     :slant italic)))
   `(doom-modeline-urgent                     ((,class                                                     :inherit error)))
   `(doom-modeline-vpsc-space-face            ((,class                                                     :inherit variable-pitch)))
   `(doom-modeline-warning                    ((,class                                                     :inherit warning)))

;;;;; elfeed
   `(elfeed-log-error-level-face              ((,class :foreground ,hr-red)))
   `(elfeed-log-info-level-face               ((,class :foreground ,hr-blue)))
   `(elfeed-log-warn-level-face               ((,class :foreground ,hr-yellow)))
   `(elfeed-search-date-face                  ((,class :foreground ,hr-blue)))
   `(elfeed-search-feed-face                  ((,class :foreground ,hr-cyan)))
   `(elfeed-search-tag-face                   ((,class :foreground ,hr-green)))

;;;;; evil
   `(evil-ex-info                             ((,class :foreground ,hr-red)))
   `(evil-ex-lazy-highlight                   ((,class                                                     :inherit lazy-highlight)))
   `(evil-ex-search                           ((,class                                                     :inherit isearch)))
   `(evil-ex-substitute-replacement           ((,class :foreground ,hr-red                                 :underline t)))
   `(evil-ex-substitute-matches               ((,class                                                     :inherit isearch)))

;;;;; flycheck
   `(flycheck-error                           ((,class                                                     :underline (:color ,hr-red    :style wave))))
   `(flycheck-info                            ((,class                                                     :underline (:color ,hr-cyan   :style wave))))
   `(flycheck-warning                         ((,class                                                     :underline (:color ,hr-orange :style wave))))
   `(flycheck-fringe-error                    ((,class :foreground ,hr-red)))
   `(flycheck-fringe-info                     ((,class :foreground ,hr-cyan)))
   `(flycheck-fringe-warning                  ((,class :foreground ,hr-orange)))

;;;;; hydra
   `(hydra-face-amaranth                      ((,class :foreground ,hr-orange                              :weight bold)))
   `(hydra-face-blue                          ((,class :foreground ,hr-blue :weight bold)))
   `(hydra-face-pink                          ((,class :foreground ,hr-yellow :weight bold)))
   `(hydra-face-red                           ((,class :foreground ,hr-red                                 :weight bold)))
   `(hydra-face-teal                          ((,class :foreground ,hr-cyan                                :weight bold)))

;;;;; ido-vertical-mode
   `(ido-vertical-match-face                  ((,class :foreground ,hr-yellow                              :weight bold :underline t)))

;;;;; linum-relative
   `(linum-relative-current-face              ((,class :foreground ,hr-yellow    :background ,hr-black-2   :weight bold)))

;;;;; lsp-ui
   `(lsp-ui-doc-background                    ((,class                           :background ,hr-black-3)))
   `(lsp-ui-doc-border                        ((,class                                                     :inherit border)))
   `(lsp-ui-doc-header                        ((,class :foreground ,hr-bg        :background ,hr-blue)))
   `(lsp-ui-doc-url                           ((,class :foreground ,hr-blue      :background ,hr-black-3)))
   `(lsp-ui-peek-filename                     ((,class :foreground ,hr-magenta)))
   `(lsp-ui-peek-footer                       ((,class :foreground ,hr-bg        :background ,hr-black-3)))
   `(lsp-ui-peek-header                       ((,class :foreground ,hr-fg        :background ,hr-black-6)))
   `(lsp-ui-peek-highlight                    ((,class :foreground ,hr-blue      :background ,hr-black-3)))
   `(lsp-ui-peek-line-number                  ((,class                           :background ,hr-black-3   :inherit line-number)))
   `(lsp-ui-peek-list                         ((,class :foreground ,hr-fg        :background ,hr-black-3)))
   `(lsp-ui-peek-peek                         ((,class                           :background ,hr-black-3)))
   `(lsp-ui-peek-selection                    ((,class :foreground ,hr-yellow    :background ,hr-black-6)))
   `(lsp-ui-sideline-code-action              ((,class :foreground ,hr-blue                                :weight bold)))
   `(lsp-ui-sideline-current-symbol           ((,class :foreground ,hr-yellow                              :weight bold)))
   `(lsp-ui-sideline-symbol                   ((,class :foreground ,hr-red       :background ,hr-bg)))
   `(lsp-ui-sideline-symbol-info              ((,class :foreground ,hr-cyan      :background ,hr-bg)))

;;;;; magit
   `(magit-branch-current                     ((,class :foreground ,hr-blue                                :box t)))
   `(magit-branch-local                       ((,class :foreground ,hr-blue)))
   `(magit-branch-remote                      ((,class :foreground ,hr-cyan)))
   `(magit-branch-remote-head                 ((,class :foreground ,hr-cyan                                :box t)))
   `(magit-branch-upstream                    ((,class                                                     :slant italic)))
   `(magit-diff-added                         ((,class :foreground ,hr-green     :background ,hr-black-2)))
   `(magit-diff-added-highlight               ((,class :foreground ,hr-green     :background ,hr-black-3)))
   `(magit-diff-base                          ((,class :foreground ,hr-bg        :background ,hr-green)))
   `(magit-diff-base-highlight                ((,class :foreground ,hr-bg        :background ,hr-green)))
   `(magit-diff-conflict-heading              ((,class                                                     :inherit magit-diff-hunk-heading)))
   `(magit-diff-context                       ((,class                           :background ,hr-black-2)))
   `(magit-diff-context-highlight             ((,class                           :background ,hr-black-3)))
   `(magit-diff-file-heading                  ((,class                           :background ,hr-bg        :weight bold)))
   `(magit-diff-file-heading-highlight        ((,class                           :background ,hr-black-3)))
   `(magit-diff-file-heading-selection        ((,class :foreground ,hr-orange                              :inherit magit-diff-file-heading-highlight)))
   `(magit-diff-hunk-heading                  ((,class                           :background ,hr-black-3)))
   `(magit-diff-hunk-heading-highlight        ((,class                           :background ,hr-black-6)))
   `(magit-diff-hunk-heading-selection        ((,class :foreground ,hr-orange                              :inherit magit-diff-hunk-heading-highlight)))
   `(magit-diff-hunk-region                   ((,class :foreground ,hr-fg                                  :weight bold)))
   `(magit-diff-lines-boundary                ((,class                                                     :inherit magit-diff-lines-heading)))
   `(magit-diff-lines-heading                 ((,class :foreground ,hr-bg        :background ,hr-orange)))
   `(magit-diff-our                           ((,class                                                     :inherit magit-diff-removed)))
   `(magit-diff-our-highlight                 ((,class                                                     :inherit magit-diff-removed-highlight)))
   `(magit-diff-removed                       ((,class :foreground ,hr-red       :background ,hr-black-2)))
   `(magit-diff-removed-highlight             ((,class :foreground ,hr-red       :background ,hr-black-3)))
   `(magit-diff-revision-summary              ((,class                                                     :inherit magit-diff-hunk-heading)))
   `(magit-diff-revision-summary-highlight    ((,class                                                     :inherit magit-diff-hunk-heading-highlight)))
   `(magit-diff-their                         ((,class                                                     :inherit magit-diff-added)))
   `(magit-diff-their-highlight               ((,class                                                     :inherit magit-diff-added-highlight)))
   `(magit-diff-whitespace-warning            ((,class                                                     :inherit trailing-whitespace)))
   `(magit-diffstat-added                     ((,class :foreground ,hr-green)))
   `(magit-diffstat-removed                   ((,class :foreground ,hr-red)))
   `(magit-dimmed                             ((,class                                                     :inherit font-lock-comment-face)))
   `(magit-filename                           ((,class :foreground ,hr-fg)))
   `(magit-hash                               ((,class :foreground ,hr-blue)))
   `(magit-head                               ((,class                                                     :inherit magit-branch-local)))
   `(magit-header-line                        ((,class :foreground ,hr-yellow                              :weight bold)))
   `(magit-header-line-key                    ((,class :foreground ,hr-yellow)))
   `(magit-header-line-log-select             ((,class :foreground ,hr-fg                                  :weight bold)))
   `(magit-keyword                            ((,class :foreground ,hr-yellow)))
   `(magit-keyword-squash                     ((,class :foreground ,hr-orange)))
   `(magit-log-author                         ((,class :foreground ,hr-cyan)))
   `(magit-log-date                           ((,class :foreground ,hr-green)))
   `(magit-log-graph                          ((,class :foreground ,hr-fg)))
   `(magit-mode-line-process                  ((,class                                                     :inherit mode-line-emphasis)))
   `(magit-mode-line-process-error            ((,class :foreground ,hr-red                                 :weight bold)))
   `(magit-process-ng                         ((,class :foreground ,hr-red                                 :weight bold)))
   `(magit-process-ok                         ((,class :foreground ,hr-green                               :weight bold)))
   `(magit-reflog-amend                       ((,class :foreground ,hr-magenta)))
   `(magit-reflog-checkout                    ((,class :foreground ,hr-blue)))
   `(magit-reflog-cherry-pick                 ((,class :foreground ,hr-green)))
   `(magit-reflog-commit                      ((,class :foreground ,hr-green)))
   `(magit-reflog-merge                       ((,class :foreground ,hr-green)))
   `(magit-reflog-other                       ((,class :foreground ,hr-cyan)))
   `(magit-reflog-rebase                      ((,class :foreground ,hr-magenta)))
   `(magit-reflog-remote                      ((,class :foreground ,hr-cyan)))
   `(magit-reflog-reset                       ((,class :foreground ,hr-red)))
   `(magit-section-heading                    ((,class :foreground ,hr-yellow                              :weight bold)))
   `(magit-section-heading-selection          ((,class :foreground ,hr-orange                              :weight bold)))
   `(magit-section-highlight                  ((,class                           :background ,hr-black-3)))
   `(magit-section-secondary-heading          ((,class :foreground ,hr-fg                                  :weight bold)))
   `(magit-sequence-done                      ((,class                                                     :inherit magit-hash)))
   `(magit-sequence-drop                      ((,class :foreground ,hr-red)))
   `(magit-sequence-exec                      ((,class                                                     :inherit magit-hash)))
   `(magit-sequence-head                      ((,class :foreground ,hr-blue-1)))
   `(magit-sequence-onto                      ((,class                                                     :inherit magit-sequence-done)))
   `(magit-sequence-part                      ((,class :foreground ,hr-green)))
   `(magit-sequence-pick                      ((,class :foreground ,hr-fg)))
   `(magit-sequence-stop                      ((,class :foreground ,hr-cyan)))
   `(magit-signature-bad                      ((,class :foreground ,hr-red                                 :weight bold)))
   `(magit-signature-error                    ((,class :foreground ,hr-red)))
   `(magit-signature-expired                  ((,class :foreground ,hr-orange)))
   `(magit-signature-expired-key              ((,class :foreground ,hr-orange)))
   `(magit-signature-good                     ((,class :foreground ,hr-green)))
   `(magit-signature-revoked                  ((,class :foreground ,hr-red+1)))
   `(magit-signature-untrusted                ((,class :foreground ,hr-cyan)))
   `(magit-tag                                ((,class :foreground ,hr-orange)))

;;;;; mu4e
   `(mu4e-attach-number-face                  ((,class :foreground ,hr-yellow                              :weight bold)))
   `(mu4e-cited-1-face                        ((,class :foreground ,hr-white+5                             :slant italic)))
   `(mu4e-cited-2-face                        ((,class :foreground ,hr-white+6                             :slant italic)))
   `(mu4e-cited-3-face                        ((,class :foreground ,hr-white+5                             :slant italic)))
   `(mu4e-cited-4-face                        ((,class :foreground ,hr-white+6                             :slant italic)))
   `(mu4e-cited-5-face                        ((,class :foreground ,hr-white+5                             :slant italic)))
   `(mu4e-cited-6-face                        ((,class :foreground ,hr-white+6                             :slant italic)))
   `(mu4e-cited-7-face                        ((,class :foreground ,hr-fg                                  :slant italic)))
   `(mu4e-compose-header-face                 ((,class                                                     :inherit font-lock-comment-face)))
   `(mu4e-compose-separator-face              ((,class                                                     :inherit font-lock-comment-face)))
   `(mu4e-contact-face                        ((,class :foreground ,hr-red)))
   `(mu4e-context-face                        ((,class :foreground ,hr-orange    :background ,hr-black-3)))
   `(mu4e-draft-face                          ((,class :foreground ,hr-green)))
   `(mu4e-flagged-face                        ((,class                                                     :inherit font-lock-constant-face)))
   `(mu4e-footer-face                         ((,class                                                     :inherit font-lock-comment-face)))
   `(mu4e-forwarded-face                      ((,class :foreground ,hr-blue)))
   `(mu4e-header-face                         ((,class :foreground ,hr-fg)))
   `(mu4e-header-highlight-face               ((,class                           :background ,hr-black-3)))
   `(mu4e-header-key-face                     ((,class :foreground ,hr-blue                                :weight bold)))
   `(mu4e-header-marks-face                   ((,class :foreground ,hr-orange                              :weight bold)))
   `(mu4e-header-title-face                   ((,class :foreground ,hr-orange)))
   `(mu4e-header-value-face                   ((,class :foreground ,hr-green)))
   `(mu4e-highlight-face                      ((,class :foreground ,hr-yellow)))
   `(mu4e-link-face                           ((,class                                                     :inherit link)))
   `(mu4e-modeline-face                       ((,class                                                     :inherit mode-line)))
   `(mu4e-moved-face                          ((,class :foreground ,hr-cyan)))
   `(mu4e-ok-face                             ((,class :foreground ,hr-green                               :weight bold)))
   `(mu4e-region-code                         ((,class :foreground ,hr-fg        :background ,hr-black-3)))
   `(mu4e-replied-face                        ((,class :foreground ,hr-white+6)))
   `(mu4e-special-header-value-face           ((,class :foreground ,hr-cyan)))
   `(mu4e-system-face                         ((,class                                                     :inherit font-lock-comment-face)))
   `(mu4e-title-face                          ((,class :foreground ,hr-orange                              :bold)))
   `(mu4e-trashed-face                        ((,class :foreground ,hr-black-3                             :strike-through t)))
   `(mu4e-unread-face                         ((,class :foreground ,hr-white                               :weight bold)))
   `(mu4e-url-number-face                     ((,class :foreground ,hr-yellow                              :weight bold)))
   `(mu4e-view-body-face                      ((,class :background ,hr-bg)))
   `(mu4e-warning-face                        ((,class                                                     :inherit warning)))

;;;;; neotree
   `(neo-banner-face                          ((,class :foreground ,hr-orange                              :weight bold)))
   `(neo-button-face                          ((,class                                                     :underline t)))
   `(neo-dir-link-face                        ((,class :foreground ,hr-magenta)))
   `(neo-expand-btn-face                      ((,class :foreground ,hr-black-7)))
   `(neo-file-link-face                       ((,class :foreground ,hr-fg)))
   `(neo-header-face                          ((,class :foreground ,hr-fg        :background ,hr-black-3)))
   `(neo-root-dir-face                        ((,class :foreground ,hr-orange)))
   `(neo-vc-added-face                        ((,class :foreground ,hr-green)))
   `(neo-vc-conflict-face                     ((,class :foreground ,hr-red)))
   `(neo-vc-default-face                      ((,class :foreground ,hr-fg)))
   `(neo-vc-edited-face                       ((,class :foreground ,hr-blue)))
   `(neo-vc-ignored-face                      ((,class :foreground ,hr-white+6)))
   `(neo-vc-missing-face                      ((,class :foreground ,hr-red)))
   `(neo-vc-needs-merge-face                  ((,class :foreground ,hr-red)))
   `(neo-vc-unlocked-changes-face             ((,class :foreground ,hr-orange                              :slant italic)))
   `(neo-vc-user-face                         ((,class :foreground ,hr-red                                 :slant italic)))

;;;;; org-roam
   `(org-roam-link                            ((,class                                                     :inherit org-link)))
   `(org-roam-link-current                    ((,class                                                     :inherit org-link)))
   `(org-roam-link-invalid                    ((,class :foreground ,hr-red                                 :inherit org-link)))
   `(org-roam-link-shielded                   ((,class :foreground ,hr-orange                              :inherit org-link)))

;;;;; paradox
   `(paradox-archive-face                     ((,class                                                     :inherit paradox-comment-face)))
   `(paradox-comment-face                     ((,class :foreground ,hr-white+6)))
   `(paradox-commit-tag-face                  ((,class :foreground ,hr-orange)))
   `(paradox-description-face                 ((,class :foreground ,hr-fg)))
   `(paradox-description-face-multiline       ((,class :foreground ,hr-white+6)))
   `(paradox-download-face                    ((,class :foreground ,hr-blue)))
   `(paradox-highlight-face                   ((,class :foreground ,hr-orange                              :weight bold)))
   `(paradox-homepage-button-face             ((,class :foreground ,hr-white+6                             :underline t)))
   `(paradox-mode-line-face                   ((,class :foreground ,hr-blue                                :weight bold)))
   `(paradox-name-face                        ((,class :foreground ,hr-blue                                :underline t)))
   `(paradox-star-face                        ((,class :foreground ,hr-yellow)))
   `(paradox-starred-face                     ((,class :foreground ,hr-orange)))

;;;;; php-mode
   `(php-$this                                ((,class :foreground ,hr-red)))
   `(php-$this-sigil                          ((,class :foreground ,hr-cyan)))
   `(php-class-declaration                    ((,class :foreground ,hr-cyan)))
   `(php-class-declaration-spec               ((,class :foreground ,hr-cyan)))
   `(php-class-modifier                       ((,class :foreground ,hr-cyan)))
   `(php-constant                             ((,class :foreground ,hr-magenta)))
   `(php-doc-$this                            ((,class :foreground ,hr-red)))
   `(php-doc-$this-sigil                      ((,class :foreground ,hr-cyan )))
   `(php-doc-variable-sigil                   ((,class :foreground ,hr-cyan)))
   `(php-errorcontrol-op                      ((,class :foreground ,hr-fg)))
   `(php-function-call                        ((,class :foreground ,hr-fg)))
   `(php-function-name                        ((,class :foreground ,hr-fg)))
   `(php-import-declaration                   ((,class :foreground ,hr-cyan)))
   `(php-keyword                              ((,class :foreground ,hr-blue)))
   `(php-magical-constant                     ((,class :foreground ,hr-magenta)))
   `(php-namespace-declaration                ((,class :foreground ,hr-cyan)))
   `(php-operator                             ((,class :foreground ,hr-blue)))
   `(php-paamayim-nekudotayim                 ((,class :foreground ,hr-cyan)))
   `(php-php-tag                              ((,class :foreground ,hr-red)))
   `(php-property-name                        ((,class :foreground ,hr-fg)))
   `(php-variable-name                        ((,class :foreground ,hr-red)))
   `(php-variable-sigil                       ((,class :foreground ,hr-cyan)))
   ;; These are declared but not yet implemented by php-mode
   `(php-builtin                              ((,class :foreground ,hr-yellow)))
   `(php-class                                ((,class :foreground ,hr-fg)))
   `(php-constant-assign                      ((,class :foreground ,hr-fg)))
   `(php-control-structure                    ((,class :foreground ,hr-blue)))
   `(php-doc-annotation-tag                   ((,class :foreground ,hr-cyan)))
   `(php-doc-class-name                       ((,class :foreground ,hr-orange)))
   `(php-method-modifier                      ((,class :foreground ,hr-orange)))
   `(php-visibility-modifier                  ((,class :foreground ,hr-orange)))

;;;;; popup
   `(popup-face                               ((,class                           :background ,hr-black-3)))
   `(popup-summary-face                       ((,class :foreground ,hr-white+6                             :inherit popup-face)))
   `(popup-scroll-bar-foreground-face         ((,class                           :background ,hr-black-6)))
   `(popup-scroll-bar-background-face         ((,class                           :background ,hr-black-3)))
   `(popup-isearch-match                      ((,class :foreground ,hr-bg        :background ,hr-yellow)))
   `(popup-tip-face                           ((,class                           :background ,hr-black-3)))
   `(popup-menu-face                          ((,class                           :background ,hr-black-3)))
   `(popup-menu-mouse-face                    ((,class :foreground ,hr-yellow    :background ,hr-black-6)))
   `(popup-menu-selection-face                ((,class :foreground ,hr-yellow    :background ,hr-black-6)))
   `(popup-menu-summary-face                  ((,class                                                     :inherit popup-summary-face)))

;;;;; powerline
   `(mode-line-buffer-id-inactive             ((,class                                                     :inherit mode-line-buffer-id)))
   `(powerline-active0                        ((,class                                                     :inherit mode-line-emphasis)))
   `(powerline-active1                        ((,class                                                     :inherit mode-line)))
   `(powerline-active2                        ((,class :foreground ,hr-fg                                  :inherit mode-line-inactive)))
   `(powerline-inactive0                      ((,class                                                     :inherit mode-line-inactive)))
   `(powerline-inactive1                      ((,class                                                     :inherit mode-line-inactive)))
   `(powerline-inactive2                      ((,class :background ,hr-bg                                  :inherit mode-line-inactive)))

;;;;; powerline-evil
   `(powerline-evil-base-face                 ((,class                                                     :weight normal)))
   `(powerline-evil-emacs-face                ((,class :foreground ,hr-black     :background ,hr-magenta   :inherit powerline-evil-base-face)))
   `(powerline-evil-insert-face               ((,class :foreground ,hr-black     :background ,hr-green     :inherit powerline-evil-base-face)))
   `(powerline-evil-motion-face               ((,class :foreground ,hr-black     :background ,hr-cyan      :inherit powerline-evil-base-face)))
   `(powerline-evil-normal-face               ((,class :foreground ,hr-black     :background ,hr-blue      :inherit powerline-evil-base-face)))
   `(powerline-evil-operator-face             ((,class :foreground ,hr-white     :background ,hr-black-8   :inherit powerline-evil-base-face)))
   `(powerline-evil-replace-face              ((,class :foreground ,hr-black     :background ,hr-orange    :inherit powerline-evil-base-face)))
   `(powerline-evil-visual-face               ((,class :foreground ,hr-black     :background ,hr-magenta   :inherit powerline-evil-base-face)))

;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face          ((,class :foreground ,hr-fg)))
   `(rainbow-delimiters-depth-2-face          ((,class :foreground ,hr-cyan)))
   `(rainbow-delimiters-depth-3-face          ((,class :foreground ,hr-yellow)))
   `(rainbow-delimiters-depth-4-face          ((,class :foreground ,hr-green+2)))
   `(rainbow-delimiters-depth-5-face          ((,class :foreground ,hr-blue)))
   `(rainbow-delimiters-depth-6-face          ((,class :foreground ,hr-fg)))
   `(rainbow-delimiters-depth-7-face          ((,class :foreground ,hr-cyan)))
   `(rainbow-delimiters-depth-8-face          ((,class :foreground ,hr-yellow)))
   `(rainbow-delimiters-depth-9-face          ((,class :foreground ,hr-green+2)))
   `(rainbow-delimiters-depth-10-face         ((,class :foreground ,hr-blue)))
   `(rainbow-delimiters-unmatched-face        ((,class :foreground ,hr-red)))

;;;;; selectrum
   `(selectrum-current-candidate              ((,class :foreground ,hr-orange                              :weight bold)))
   `(selectrum-primary-highlight              ((,class :foreground ,hr-blue)))
   `(selectrum-secondary-highlight            ((,class :foreground ,hr-red)))

;;;;; smart-mode-line
   `(sml/charging                             ((,class :foreground ,hr-fg)))
   `(sml/client                               ((,class                                                     :inherit sml/prefix)))
   `(sml/col-number                           ((,class                                                     :inherit sml/global)))
   `(sml/discharging                          ((,class :foreground ,hr-fg)))
   `(sml/filename                             ((,class                                                     :inherit mode-line-emphasis)))
   `(sml/folder                               ((,class :foreground ,hr-magenta                             :inherit mode-line-emphasis)))
   `(sml/git                                  ((,class                                                     :inherit sml/vc)))
   `(sml/global                               ((,class :foreground ,hr-fg)))
   `(sml/line-number                          ((,class                                                     :inherit sml/global)))
   `(sml/minor-modes                          ((,class :foreground ,hr-cyan)))
   `(sml/modes                                ((,class :foreground ,hr-blue                                :inherit mode-line-emphasis)))
   `(sml/modified                             ((,class :foreground ,hr-orange)))
   `(sml/mule-info                            ((,class                                                     :inherit sml/global)))
   `(sml/name-filling                         ((,class                                                     :inherit sml/position-percentage)))
   `(sml/not-modified                         ((,class                                                     :inherit sml/global)))
   `(sml/numbers-separator                    ((,class                                                     :inherit sml/global)))
   `(sml/outside-modified                     ((,class :foreground ,hr-red                                 :weight bold)))
   `(sml/position-percentage                  ((,class                                                     :inherit sml/global)))
   `(sml/prefix                               ((,class                                                     :inherit sml/global)))
   `(sml/process                              ((,class                                                     :inherit sml/prefix)))
   `(sml/projectile                           ((,class :foreground ,hr-magenta                             :weight bold)))
   `(sml/read-only                            ((,class :foreground ,hr-orange                              :weight bold)))
   `(sml/remote                               ((,class                                                     :inherit sml/global)))
   `(sml/sudo                                 ((,class :foreground ,hr-red                                 :weight bold)))
   `(sml/time                                 ((,class                                                     :inherit sml/global)))
   `(sml/vc                                   ((,class :foreground ,hr-cyan                                :weight bold)))
   `(sml/vc-edited                            ((,class :foreground ,hr-orange                              :weight bold)))

;;;;; spaceline
   `(spaceline-evil-emacs                     ((,class :foreground ,hr-black     :background ,hr-magenta)))
   `(spaceline-evil-insert                    ((,class :foreground ,hr-black     :background ,hr-green)))
   `(spaceline-evil-motion                    ((,class :foreground ,hr-black     :background ,hr-cyan)))
   `(spaceline-evil-normal                    ((,class :foreground ,hr-black     :background ,hr-blue)))
   `(spaceline-evil-replace                   ((,class :foreground ,hr-black     :background ,hr-orange)))
   `(spaceline-evil-visual                    ((,class :foreground ,hr-black     :background ,hr-magenta)))
   `(spaceline-flycheck-error                 ((,class :foreground ,hr-red)))
   `(spaceline-flycheck-info                  ((,class :foreground ,hr-cyan)))
   `(spaceline-flycheck-warning               ((,class :foreground ,hr-orange)))
   `(spaceline-highlight-face                 ((,class :foreground ,hr-black     :background ,hr-orange)))
   `(spaceline-modified                       ((,class                                                     :inherit spaceline-evil-normal)))
   `(spaceline-python-venv                    ((,class :foreground ,hr-magenta)))
   `(spaceline-read-only                      ((,class                                                     :inherit mode-line)))
   `(spaceline-unmodified                     ((,class :foreground ,hr-black     :background ,hr-orange)))

;;;;; switch-window
   `(switch-window-background                 ((,class :foreground ,hr-black-2)))
   `(switch-window-label                      ((,class :foreground ,hr-yellow                              :weight bold :height 3.0)))

;;;;; telephone-line
   `(telephone-line-accent-active             ((,class :foreground ,hr-fg        :background ,hr-black-5)))
   `(telephone-line-accent-inactive           ((,class :foreground ,hr-black-8   :background ,hr-black-3)))
   `(telephone-line-error                     ((,class                                                     :inherit error)))
   `(telephone-line-evil                      ((,class                                                     :weight normal)))
   `(telephone-line-evil-emacs                ((,class :foreground ,hr-black     :background ,hr-magenta   :inherit telephone-line-evil)))
   `(telephone-line-evil-insert               ((,class :foreground ,hr-black     :background ,hr-green     :inherit telephone-line-evil)))
   `(telephone-line-evil-motion               ((,class :foreground ,hr-black     :background ,hr-cyan      :inherit telephone-line-evil)))
   `(telephone-line-evil-normal               ((,class :foreground ,hr-black     :background ,hr-blue      :inherit telephone-line-evil)))
   `(telephone-line-evil-operator             ((,class :foreground ,hr-white     :background ,hr-black-8   :inherit telephone-line-evil)))
   `(telephone-line-evil-replace              ((,class :foreground ,hr-black     :background ,hr-orange    :inherit telephone-line-evil)))
   `(telephone-line-evil-visual               ((,class :foreground ,hr-black     :background ,hr-magenta   :inherit telephone-line-evil)))
   `(telephone-line-projectile                ((,class :foreground ,hr-fg)))
   `(telephone-line-unimportant               ((,class :foreground ,hr-white+6                             :inherit mode-line)))
   `(telephone-line-warning                   ((,class :foreground ,hr-orange                              :weight normal)))

;;;;; transient
   `(transient-active-infix                   ((,class :foreground ,hr-fg        :background ,hr-black-3)))
   `(transient-argument                       ((,class :foreground ,hr-orange                              :weight bold)))
   `(transient-disabled-suffix                ((,class :foreground ,hr-bg        :background ,hr-red       :weight bold)))
   `(transient-enabled-suffix                 ((,class :foreground ,hr-bg        :background ,hr-green     :weight bold)))
   `(transient-heading                        ((,class :foreground ,hr-blue)))
   `(transient-inactive-argument              ((,class :foreground ,hr-blue)))
   `(transient-inactive-value                 ((,class :foreground ,hr-blue)))
   `(transient-inapt-suffix                   ((,class :foreground ,hr-fg)))
   `(transient-key                            ((,class :foreground ,hr-yellow)))
   `(transient-mismatched-key                 ((,class :foreground ,hr-fg                                  :underline t)))
   `(transient-nonstandard-key                ((,class :foreground ,hr-fg                                  :underline t)))
   `(transient-separator                      ((,class :foreground ,hr-fg        :background ,hr-black-6)))
   `(transient-unreachable                    ((,class :foreground ,hr-fg)))
   `(transient-unreachable-key                ((,class :foreground ,hr-fg)))
   `(transient-value                          ((,class :foreground ,hr-magenta)))

;;;;; treemacs
   `(treemacs-directory-collapsed-face        ((,class :foreground ,hr-magenta)))
   `(treemacs-directory-face                  ((,class :foreground ,hr-magenta)))
   `(treemacs-file-face                       ((,class :foreground ,hr-fg)))
   `(treemacs-fringe-indicator-face           ((,class :foreground ,hr-fg)))
   `(treemacs-git-added-face                  ((,class :foreground ,hr-green)))
   `(treemacs-git-conflict-face               ((,class :foreground ,hr-red)))
   `(treemacs-git-ignored-face                ((,class :foreground ,hr-white+6)))
   `(treemacs-git-modified-face               ((,class :foreground ,hr-blue)))
   `(treemacs-git-renamed-face                ((,class :foreground ,hr-cyan)))
   `(treemacs-git-unmodified-face             ((,class :foreground ,hr-fg)))
   `(treemacs-git-untracked-face              ((,class :foreground ,hr-white+5)))
   `(treemacs-header-button-face              ((,class :foreground ,hr-blue                                :underline t)))
   `(treemacs-help-column-face                ((,class :foreground ,hr-yellow)))
   `(treemacs-help-title-face                 ((,class :foreground ,hr-blue)))
   `(treemacs-on-failure-pulse-face           ((,class :foreground ,hr-bg        :background ,hr-red)))
   `(treemacs-on-success-pulse-face           ((,class :foreground ,hr-bg        :background ,hr-green)))
   `(treemacs-root-face                       ((,class :foreground ,hr-orange)))
   `(treemacs-root-remote-disconnected-face   ((,class :foreground ,hr-yellow                              :weight bold)))
   `(treemacs-root-remote-face                ((,class :foreground ,hr-yellow)))
   `(treemacs-root-remote-unreadable-face     ((,class                                                     :inherit treemacs-root-remote-face :strike-through t)))
   `(treemacs-root-unreadable-face            ((,class                                                     :inherit treemacs-root-face :strike-through t)))
   `(treemacs-tags-face                       ((,class :foreground ,hr-blue)))
   `(treemacs-term-node-face                  ((,class :foreground ,hr-green)))

;;;;; vterm
   `(vterm-color-black                        ((,class :foreground ,hr-black-3   :background ,hr-black-6)))
   `(vterm-color-blue                         ((,class :foreground ,hr-blue+1    :background ,hr-blue)))
   `(vterm-color-cyan                         ((,class :foreground ,hr-cyan+1    :background ,hr-cyan)))
   `(vterm-color-default                      ((,class :foreground ,hr-fg        :background ,hr-bg)))
   `(vterm-color-green                        ((,class :foreground ,hr-green+1   :background ,hr-green)))
   `(vterm-color-inverse-video                ((,class                           :background ,hr-bg        :inverse-video t)))
   `(vterm-color-magenta                      ((,class :foreground ,hr-magenta+1 :background ,hr-magenta)))
   `(vterm-color-red                          ((,class :foreground ,hr-red+1     :background ,hr-red)))
   `(vterm-color-underline                    ((,class                                                     :underline t)))
   `(vterm-color-white                        ((,class :foreground ,hr-white+6   :background ,hr-fg)))
   `(vterm-color-yellow                       ((,class :foreground ,hr-orange    :background ,hr-yellow)))

;;;;; which-key
   `(which-key-command-description-face       ((,class :foreground ,hr-blue)))
   `(which-key-docstring-face                 ((,class :foreground ,hr-white+6)))
   `(which-key-group-description-face         ((,class :foreground ,hr-orange)))
   `(which-key-highlighted-command-face       ((,class :foreground ,hr-blue)))
   `(which-key-key-face                       ((,class :foreground ,hr-yellow)))
   `(which-key-local-map-description-face     ((,class :foreground ,hr-blue)))
   `(which-key-note-face                      ((,class :foreground ,hr-white+6)))
   `(which-key-separator-face                 ((,class :foreground ,hr-white+6)))
   `(which-key-special-key-face               ((,class :foreground ,hr-yellow)))

;;;;; winum
   `(winum-face                               ((,class :foreground ,hr-cyan                                :weight bold)))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'hybrid-reverse)

(provide 'hybrid-reverse-theme)

;;; hybrid-reverse-theme.el ends here
