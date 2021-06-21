;;; hybrid-theme.el --- A low contrast color theme for Emacs.

;; Copyright (C) 2017 Evan Bergeron

;; Author: Evan Bergeron <bergeronej@gmail.com>
;; URL: http://github.com/evanbergeron/hybrid-emacs
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A port of the popular Vim theme Hybrid for Emacs 24+, built on top
;; of the new built-in theme support in Emacs 24.

;;; Credits:

;; The code is essentially a gradually-more-reskinned-zenburn, so much
;; credit is due to Bozhidar Batsov, Jani Nurminen, and every
;; contributor to the zenburn theme.

;; TODO highlight numeral literals red
;; TODO get rid of all the GUI green text - replace with blue or gray

;;; Code:

(deftheme hybrid "The Hybrid color theme")

;;; Color Palette

(defvar hybrid-default-colors-alist
  '(("hybrid-fg+1"     . "#c5c8c6")
    ("hybrid-fg"       . "#c5c8c6")
    ("hybrid-fg-1"     . "#c5c8c6")
    ("hybrid-bg-2"     . "#232c31")
    ("hybrid-bg-1"     . "#232c31")
    ("hybrid-bg-05"    . "#232c31")
    ("hybrid-bg"       . "#232c31")
    ("hybrid-bg+05"    . "#232c31")
    ("hybrid-bg+1"     . "#232c31")
    ("hybrid-bg+2"     . "#232c31")
    ("hybrid-bg+3"     . "#232c31")
    ("hybrid-red+1"    . "#DCA3A3")
    ("hybrid-red"      . "#cc6666")
    ("hybrid-red-1"    . "#cc6666")
    ("hybrid-red-2"    . "#cc6666")
    ("hybrid-red-3"    . "#5f0000")
    ("hybrid-red-4"    . "#5f0000")
    ("hybrid-orange"   . "#de935f")
    ("hybrid-yellow"   . "#f0c674")
    ("hybrid-yellow-1" . "#f0c674")
    ("hybrid-yellow-2" . "#f0c674")
    ("hybrid-green-1"  . "#5F7F5F")
    ("hybrid-green"    . "#b5bd68")
    ("hybrid-green+1"  . "#b5bd68")
    ("hybrid-green+2"  . "#b5bd68")
    ("hybrid-green+3"  . "#b5bd68")
    ("hybrid-green+4"  . "#b5bd68")
    ("hybrid-cyan"     . "#005f5f")
    ("hybrid-blue+1"   . "#94BFF3")
    ("hybrid-blue"     . "#81a2be")
    ("hybrid-blue-1"   . "#81a2be")
    ("hybrid-blue-2"   . "#81a2be")
    ("hybrid-blue-3"   . "#81a2be")
    ("hybrid-blue-4"   . "#00005f")
    ("hybrid-blue-5"   . "#00005f")
    ("hybrid-comment"  . "#6c7a80")
    ("hybrid-region"   . "#434c51")
    ("hybrid-magenta"  . "#b294bb"))
  "List of Hybrid colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defvar hybrid-override-colors-alist
  '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist before loading the theme.")

(defvar hybrid-colors-alist
  (append hybrid-default-colors-alist hybrid-override-colors-alist))

(defmacro hybrid-with-color-variables (&rest body)
  "`let' bind all colors defined in `hybrid-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   hybrid-colors-alist))
     ,@body))

;;; Theme Faces
(hybrid-with-color-variables
  (custom-theme-set-faces
   'hybrid
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,hybrid-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,hybrid-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,hybrid-fg :background ,hybrid-bg))))
   `(cursor ((t (:foreground ,hybrid-fg :background ,hybrid-fg+1))))
   `(escape-glyph ((t (:foreground ,hybrid-yellow :weight bold))))
   `(fringe ((t (:foreground ,hybrid-fg :background ,hybrid-bg+1))))
   `(header-line ((t (:foreground ,hybrid-yellow
                                  :background ,hybrid-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,hybrid-bg-05))))
   `(success ((t (:foreground ,hybrid-green :weight bold))))
   `(warning ((t (:foreground ,hybrid-orange :weight bold))))
   `(tooltip ((t (:foreground ,hybrid-fg :background ,hybrid-bg+1))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,hybrid-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,hybrid-green))))
   `(compilation-error-face ((t (:foreground ,hybrid-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,hybrid-fg))))
   `(compilation-info-face ((t (:foreground ,hybrid-blue))))
   `(compilation-info ((t (:foreground ,hybrid-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,hybrid-green))))
   `(compilation-line-face ((t (:foreground ,hybrid-yellow))))
   `(compilation-line-number ((t (:foreground ,hybrid-yellow))))
   `(compilation-message-face ((t (:foreground ,hybrid-blue))))
   `(compilation-warning-face ((t (:foreground ,hybrid-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,hybrid-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,hybrid-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,hybrid-yellow :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,hybrid-fg-1))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,hybrid-fg))))
   `(grep-error-face ((t (:foreground ,hybrid-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,hybrid-blue))))
   `(grep-match-face ((t (:foreground ,hybrid-orange :weight bold))))
   `(match ((t (:background ,hybrid-bg-1 :foreground ,hybrid-orange :weight bold))))
;;;;; info
   `(Info-quoted ((t (:inherit font-lock-constant-face))))
;;;;; isearch
   `(isearch ((t (:foreground ,hybrid-yellow-2 :weight bold :background ,hybrid-bg+2))))
   `(isearch-fail ((t (:foreground ,hybrid-fg :background ,hybrid-red-4))))
   `(lazy-highlight ((t (:foreground ,hybrid-yellow-2 :weight bold :background ,hybrid-bg-05))))

   `(menu ((t (:foreground ,hybrid-fg :background ,hybrid-bg))))
   `(minibuffer-prompt ((t (:foreground ,hybrid-yellow))))
   `(mode-line
     ((,class (:foreground ,hybrid-green+1
                           :background ,hybrid-region
                           :overline ,hybrid-region
                           :underline ,hybrid-region
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,hybrid-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,hybrid-green-1
                      :background ,hybrid-bg-05
                      :overline ,hybrid-region
                      :underline ,hybrid-region
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,hybrid-region))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,hybrid-bg+2))))
   `(trailing-whitespace ((t (:background ,hybrid-red))))
   `(vertical-border ((t (:foreground ,hybrid-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,hybrid-fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,hybrid-comment))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,hybrid-comment))))
   `(font-lock-constant-face ((t (:foreground ,hybrid-green+4))))
   `(font-lock-doc-face ((t (:foreground ,hybrid-green+2))))
   `(font-lock-function-name-face ((t (:foreground ,hybrid-yellow))))
   `(font-lock-keyword-face ((t (:foreground ,hybrid-blue :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,hybrid-red :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,hybrid-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,hybrid-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,hybrid-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,hybrid-green))))
   `(font-lock-type-face ((t (:foreground ,hybrid-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,hybrid-orange))))
   `(font-lock-warning-face ((t (:foreground ,hybrid-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; man
   '(Man-overstrike ((t (:inherit font-lock-keyword-face))))
   '(Man-underline  ((t (:inherit (font-lock-string-face underline)))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,hybrid-fg))))
   `(newsticker-default-face ((t (:foreground ,hybrid-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,hybrid-green+3))))
   `(newsticker-extra-face ((t (:foreground ,hybrid-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,hybrid-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,hybrid-green))))
   `(newsticker-new-item-face ((t (:foreground ,hybrid-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,hybrid-red))))
   `(newsticker-old-item-face ((t (:foreground ,hybrid-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,hybrid-fg))))
   `(newsticker-treeview-face ((t (:foreground ,hybrid-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,hybrid-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,hybrid-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,hybrid-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,hybrid-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,hybrid-bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,hybrid-bg-1 :foreground ,hybrid-yellow))))
;;;;; woman
   '(woman-bold   ((t (:inherit font-lock-keyword-face))))
   '(woman-italic ((t (:inherit (font-lock-string-face italic)))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,hybrid-fg-1 :background ,hybrid-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,hybrid-green+2 :background ,hybrid-bg :inverse-video nil))))
;;;;; ace-window
   `(aw-background-face
     ((t (:foreground ,hybrid-fg-1 :background ,hybrid-bg :inverse-video nil))))
   `(aw-leading-char-face ((t (:inherit aw-mode-line-face))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,hybrid-green+1))))
   `(android-mode-error-face ((t (:foreground ,hybrid-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,hybrid-fg))))
   `(android-mode-verbose-face ((t (:foreground ,hybrid-green))))
   `(android-mode-warning-face ((t (:foreground ,hybrid-yellow))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,hybrid-cyan :weight bold))))
   `(anzu-mode-line-no-match ((t (:foreground ,hybrid-red :weight bold))))
   `(anzu-match-1 ((t (:foreground ,hybrid-bg :background ,hybrid-green))))
   `(anzu-match-2 ((t (:foreground ,hybrid-bg :background ,hybrid-orange))))
   `(anzu-match-3 ((t (:foreground ,hybrid-bg :background ,hybrid-blue))))
   `(anzu-replace-to ((t (:inherit anzu-replace-highlight :foreground ,hybrid-yellow))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,hybrid-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,hybrid-yellow))))
   `(font-latex-italic-face ((t (:foreground ,hybrid-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,hybrid-orange))))
;;;;; agda-mode
   `(agda2-highlight-keyword-face ((t (:foreground ,hybrid-yellow :weight bold))))
   `(agda2-highlight-string-face ((t (:foreground ,hybrid-red))))
   `(agda2-highlight-symbol-face ((t (:foreground ,hybrid-orange))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,hybrid-blue-1))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,hybrid-fg))))
   `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,hybrid-fg))))
   `(agda2-highlight-datatype-face ((t (:foreground ,hybrid-blue))))
   `(agda2-highlight-function-face ((t (:foreground ,hybrid-blue))))
   `(agda2-highlight-module-face ((t (:foreground ,hybrid-blue-1))))
   `(agda2-highlight-error-face ((t (:foreground ,hybrid-bg :background ,hybrid-magenta))))
   `(agda2-highlight-unsolved-meta-face ((t (:foreground ,hybrid-bg :background ,hybrid-magenta))))
   `(agda2-highlight-unsolved-constraint-face ((t (:foreground ,hybrid-bg :background ,hybrid-magenta))))
   `(agda2-highlight-termination-problem-face ((t (:foreground ,hybrid-bg :background ,hybrid-magenta))))
   `(agda2-highlight-incomplete-pattern-face ((t (:foreground ,hybrid-bg :background ,hybrid-magenta))))
   `(agda2-highlight-typechecks-face ((t (:background ,hybrid-red-4))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,hybrid-bg+3 :foreground ,hybrid-bg-2))))
   `(ac-selection-face ((t (:background ,hybrid-blue-4 :foreground ,hybrid-fg))))
   `(popup-tip-face ((t (:background ,hybrid-yellow-2 :foreground ,hybrid-bg-2))))
   `(popup-menu-mouse-face ((t (:background ,hybrid-yellow-2 :foreground ,hybrid-bg-2))))
   `(popup-summary-face ((t (:background ,hybrid-bg+3 :foreground ,hybrid-bg-2))))
   `(popup-scroll-bar-foreground-face ((t (:background ,hybrid-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,hybrid-bg-1))))
   `(popup-isearch-match ((t (:background ,hybrid-bg :foreground ,hybrid-fg))))
;;;;; avy
   `(avy-background-face
     ((t (:foreground ,hybrid-fg-1 :background ,hybrid-bg :inverse-video nil))))
   `(avy-lead-face-0
     ((t (:foreground ,hybrid-green+3 :background ,hybrid-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-1
     ((t (:foreground ,hybrid-yellow :background ,hybrid-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-2
     ((t (:foreground ,hybrid-red+1 :background ,hybrid-bg :inverse-video nil :weight bold))))
   `(avy-lead-face
     ((t (:foreground ,hybrid-cyan :background ,hybrid-bg :inverse-video nil :weight bold))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,hybrid-fg :background ,hybrid-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,hybrid-orange :background ,hybrid-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,hybrid-orange :background ,hybrid-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,hybrid-fg :background ,hybrid-bg-1))))
   `(company-tooltip-mouse ((t (:background ,hybrid-bg-1))))
   `(company-tooltip-common ((t (:foreground ,hybrid-green+2))))
   `(company-tooltip-common-selection ((t (:foreground ,hybrid-green+2))))
   `(company-scrollbar-fg ((t (:background ,hybrid-bg-1))))
   `(company-scrollbar-bg ((t (:background ,hybrid-bg+2))))
   `(company-preview ((t (:background ,hybrid-green+2))))
   `(company-preview-common ((t (:foreground ,hybrid-green+2 :background ,hybrid-bg-1))))
;;;;; bm
   `(bm-face ((t (:background ,hybrid-yellow-1 :foreground ,hybrid-bg))))
   `(bm-fringe-face ((t (:background ,hybrid-yellow-1 :foreground ,hybrid-bg))))
   `(bm-fringe-persistent-face ((t (:background ,hybrid-green-1 :foreground ,hybrid-bg))))
   `(bm-persistent-face ((t (:background ,hybrid-green-1 :foreground ,hybrid-bg))))
;;;;; calfw
   `(cfw:face-annotation ((t (:foreground ,hybrid-red :inherit cfw:face-day-title))))
   `(cfw:face-day-title ((t nil)))
   `(cfw:face-default-content ((t (:foreground ,hybrid-green))))
   `(cfw:face-default-day ((t (:weight bold))))
   `(cfw:face-disable ((t (:foreground ,hybrid-fg-1))))
   `(cfw:face-grid ((t (:inherit shadow))))
   `(cfw:face-header ((t (:inherit font-lock-keyword-face))))
   `(cfw:face-holiday ((t (:inherit cfw:face-sunday))))
   `(cfw:face-periods ((t (:foreground ,hybrid-cyan))))
   `(cfw:face-saturday ((t (:foreground ,hybrid-blue :weight bold))))
   `(cfw:face-select ((t (:background ,hybrid-blue-5))))
   `(cfw:face-sunday ((t (:foreground ,hybrid-red :weight bold))))
   `(cfw:face-title ((t (:height 2.0 :inherit (variable-pitch font-lock-keyword-face)))))
   `(cfw:face-today ((t (:foreground ,hybrid-cyan :weight bold))))
   `(cfw:face-today-title ((t (:inherit highlight bold))))
   `(cfw:face-toolbar ((t (:background ,hybrid-blue-5))))
   `(cfw:face-toolbar-button-off ((t (:underline nil :inherit link))))
   `(cfw:face-toolbar-button-on ((t (:underline nil :inherit link-visited))))
;;;;; cider
   `(cider-result-overlay-face ((t (:background unspecified))))
   `(cider-enlightened-face ((t (:box (:color ,hybrid-orange :line-width -1)))))
   `(cider-enlightened-local-face ((t (:weight bold :foreground ,hybrid-green+1))))
   `(cider-deprecated-face ((t (:background ,hybrid-yellow-2))))
   `(cider-instrumented-face ((t (:box (:color ,hybrid-red :line-width -1)))))
   `(cider-traced-face ((t (:box (:color ,hybrid-cyan :line-width -1)))))
   `(cider-test-failure-face ((t (:background ,hybrid-red-4))))
   `(cider-test-error-face ((t (:background ,hybrid-magenta))))
   `(cider-test-success-face ((t (:background ,hybrid-green-1))))
   `(cider-fringe-good-face ((t (:foreground ,hybrid-green+4))))
;;;;; circe
   `(circe-highlight-nick-face ((t (:foreground ,hybrid-cyan))))
   `(circe-my-message-face ((t (:foreground ,hybrid-fg))))
   `(circe-fool-face ((t (:foreground ,hybrid-red+1))))
   `(circe-topic-diff-removed-face ((t (:foreground ,hybrid-red :weight bold))))
   `(circe-originator-face ((t (:foreground ,hybrid-fg))))
   `(circe-server-face ((t (:foreground ,hybrid-green))))
   `(circe-topic-diff-new-face ((t (:foreground ,hybrid-orange :weight bold))))
   `(circe-prompt-face ((t (:foreground ,hybrid-orange :background ,hybrid-bg :weight bold))))
;;;;; context-coloring
   `(context-coloring-level-0-face ((t :foreground ,hybrid-fg)))
   `(context-coloring-level-1-face ((t :foreground ,hybrid-cyan)))
   `(context-coloring-level-2-face ((t :foreground ,hybrid-green+4)))
   `(context-coloring-level-3-face ((t :foreground ,hybrid-yellow)))
   `(context-coloring-level-4-face ((t :foreground ,hybrid-orange)))
   `(context-coloring-level-5-face ((t :foreground ,hybrid-magenta)))
   `(context-coloring-level-6-face ((t :foreground ,hybrid-blue+1)))
   `(context-coloring-level-7-face ((t :foreground ,hybrid-green+2)))
   `(context-coloring-level-8-face ((t :foreground ,hybrid-yellow-2)))
   `(context-coloring-level-9-face ((t :foreground ,hybrid-red+1)))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,hybrid-blue :foreground ,hybrid-bg))))
   `(ctbl:face-continue-bar ((t (:background ,hybrid-bg-05 :foreground ,hybrid-bg))))
   `(ctbl:face-row-select ((t (:background ,hybrid-cyan :foreground ,hybrid-bg))))
;;;;; debbugs
   `(debbugs-gnu-done ((t (:foreground ,hybrid-fg-1))))
   `(debbugs-gnu-handled ((t (:foreground ,hybrid-green))))
   `(debbugs-gnu-new ((t (:foreground ,hybrid-red))))
   `(debbugs-gnu-pending ((t (:foreground ,hybrid-blue))))
   `(debbugs-gnu-stale ((t (:foreground ,hybrid-orange))))
   `(debbugs-gnu-tagged ((t (:foreground ,hybrid-red))))
;;;;; diff
   `(diff-added          ((t (:background "#335533" :foreground ,hybrid-green))))
   `(diff-changed        ((t (:background "#555511" :foreground ,hybrid-yellow-1))))
   `(diff-removed        ((t (:background "#553333" :foreground ,hybrid-red-2))))
   `(diff-refine-added   ((t (:background "#338833" :foreground ,hybrid-green+4))))
   `(diff-refine-change  ((t (:background "#888811" :foreground ,hybrid-yellow))))
   `(diff-refine-removed ((t (:background "#883333" :foreground ,hybrid-red))))
   `(diff-header ((,class (:background ,hybrid-bg+2))
                  (t (:background ,hybrid-fg :foreground ,hybrid-bg))))
   `(diff-file-header
     ((,class (:background ,hybrid-bg+2 :foreground ,hybrid-fg :weight bold))
      (t (:background ,hybrid-fg :foreground ,hybrid-bg :weight bold))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,hybrid-blue :background ,hybrid-blue-2))))
   `(diff-hl-delete ((,class (:foreground ,hybrid-red+1 :background ,hybrid-red-1))))
   `(diff-hl-insert ((,class (:foreground ,hybrid-green+1 :background ,hybrid-green-1))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,hybrid-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,hybrid-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,hybrid-orange))))
   `(diredp-date-time ((t (:foreground ,hybrid-magenta))))
   `(diredp-deletion ((t (:foreground ,hybrid-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,hybrid-red))))
   `(diredp-dir-heading ((t (:foreground ,hybrid-blue :background ,hybrid-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,hybrid-cyan))))
   `(diredp-exec-priv ((t (:foreground ,hybrid-red))))
   `(diredp-executable-tag ((t (:foreground ,hybrid-green+1))))
   `(diredp-file-name ((t (:foreground ,hybrid-blue))))
   `(diredp-file-suffix ((t (:foreground ,hybrid-green))))
   `(diredp-flag-mark ((t (:foreground ,hybrid-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,hybrid-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,hybrid-red))))
   `(diredp-link-priv ((t (:foreground ,hybrid-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,hybrid-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,hybrid-orange))))
   `(diredp-no-priv ((t (:foreground ,hybrid-fg))))
   `(diredp-number ((t (:foreground ,hybrid-green+1))))
   `(diredp-other-priv ((t (:foreground ,hybrid-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,hybrid-red-1))))
   `(diredp-read-priv ((t (:foreground ,hybrid-green-1))))
   `(diredp-symlink ((t (:foreground ,hybrid-yellow))))
   `(diredp-write-priv ((t (:foreground ,hybrid-magenta))))
;;;;; dired-async
   `(dired-async-failures ((t (:foreground ,hybrid-red :weight bold))))
   `(dired-async-message ((t (:foreground ,hybrid-yellow :weight bold))))
   `(dired-async-mode-message ((t (:foreground ,hybrid-yellow))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,hybrid-fg :background ,hybrid-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,hybrid-fg :background ,hybrid-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,hybrid-fg :background ,hybrid-green-1))))
   `(ediff-current-diff-C ((t (:foreground ,hybrid-fg :background ,hybrid-blue-5))))
   `(ediff-even-diff-A ((t (:background ,hybrid-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,hybrid-bg+1))))
   `(ediff-even-diff-B ((t (:background ,hybrid-bg+1))))
   `(ediff-even-diff-C ((t (:background ,hybrid-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,hybrid-fg :background ,hybrid-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,hybrid-fg :background ,hybrid-red-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,hybrid-fg :background ,hybrid-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,hybrid-fg :background ,hybrid-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,hybrid-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,hybrid-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,hybrid-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,hybrid-bg+2))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,hybrid-fg))))
   `(egg-help-header-1 ((t (:foreground ,hybrid-yellow))))
   `(egg-help-header-2 ((t (:foreground ,hybrid-green+3))))
   `(egg-branch ((t (:foreground ,hybrid-yellow))))
   `(egg-branch-mono ((t (:foreground ,hybrid-yellow))))
   `(egg-term ((t (:foreground ,hybrid-yellow))))
   `(egg-diff-add ((t (:foreground ,hybrid-green+4))))
   `(egg-diff-del ((t (:foreground ,hybrid-red+1))))
   `(egg-diff-file-header ((t (:foreground ,hybrid-yellow-2))))
   `(egg-section-title ((t (:foreground ,hybrid-yellow))))
   `(egg-stash-mono ((t (:foreground ,hybrid-green+4))))
;;;;; elfeed
   `(elfeed-log-error-level-face ((t (:foreground ,hybrid-red))))
   `(elfeed-log-info-level-face ((t (:foreground ,hybrid-blue))))
   `(elfeed-log-warn-level-face ((t (:foreground ,hybrid-yellow))))
   `(elfeed-search-date-face ((t (:foreground ,hybrid-yellow-1 :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,hybrid-green))))
   `(elfeed-search-feed-face ((t (:foreground ,hybrid-cyan))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,hybrid-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,hybrid-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,hybrid-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,hybrid-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,hybrid-green+2 :background ,hybrid-bg))))
   `(w3m-lnum-match ((t (:background ,hybrid-bg-1
                                     :foreground ,hybrid-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,hybrid-yellow))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,hybrid-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,hybrid-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default-face))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default-face))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,hybrid-yellow))))
   `(erc-keyword-face ((t (:foreground ,hybrid-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,hybrid-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,hybrid-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default-face))))
   `(erc-notice-face ((t (:foreground ,hybrid-green))))
   `(erc-pal-face ((t (:foreground ,hybrid-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,hybrid-orange :background ,hybrid-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,hybrid-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; eros
   `(eros-result-overlay-face ((t (:background unspecified))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,hybrid-green+4 :background ,hybrid-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,hybrid-red :background ,hybrid-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,hybrid-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,hybrid-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,hybrid-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,hybrid-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,hybrid-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,hybrid-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,hybrid-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,hybrid-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hybrid-red-1) :inherit unspecified))
      (t (:foreground ,hybrid-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hybrid-yellow) :inherit unspecified))
      (t (:foreground ,hybrid-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hybrid-cyan) :inherit unspecified))
      (t (:foreground ,hybrid-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,hybrid-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,hybrid-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,hybrid-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hybrid-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,hybrid-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hybrid-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,hybrid-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hybrid-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,hybrid-green-1 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hybrid-orange) :inherit unspecified))
      (t (:foreground ,hybrid-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hybrid-red) :inherit unspecified))
      (t (:foreground ,hybrid-red-1 :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,hybrid-fg))))
   `(ack-file ((t (:foreground ,hybrid-blue))))
   `(ack-line ((t (:foreground ,hybrid-yellow))))
   `(ack-match ((t (:foreground ,hybrid-orange :background ,hybrid-bg-1 :weight bold))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,hybrid-green+1 :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,hybrid-blue+1  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,hybrid-yellow  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,hybrid-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,hybrid-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,hybrid-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,hybrid-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,hybrid-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,hybrid-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,hybrid-magenta :weight bold))))
;;;;; git-rebase
   `(git-rebase-hash ((t (:foreground, hybrid-orange))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:weight bold :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:weight bold :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:weight bold :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:weight bold :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:weight bold :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:weight bold :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:weight bold :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:weight bold :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:weight bold :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:weight bold :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:weight bold :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:weight bold :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:weight bold :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:weight bold :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-to))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-server-opened ((t (:foreground ,hybrid-green+2 :weight bold))))
   `(gnus-server-denied ((t (:foreground ,hybrid-red+1 :weight bold))))
   `(gnus-server-closed ((t (:foreground ,hybrid-blue :slant italic))))
   `(gnus-server-offline ((t (:foreground ,hybrid-yellow :weight bold))))
   `(gnus-server-agent ((t (:foreground ,hybrid-blue :weight bold))))
   `(gnus-summary-cancelled ((t (:foreground ,hybrid-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,hybrid-blue))))
   `(gnus-summary-high-read ((t (:foreground ,hybrid-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,hybrid-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,hybrid-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,hybrid-blue))))
   `(gnus-summary-low-read ((t (:foreground ,hybrid-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,hybrid-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,hybrid-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,hybrid-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,hybrid-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,hybrid-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,hybrid-fg))))
   `(gnus-summary-selected ((t (:foreground ,hybrid-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,hybrid-blue))))
   `(gnus-cite-10 ((t (:foreground ,hybrid-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,hybrid-yellow))))
   `(gnus-cite-2 ((t (:foreground ,hybrid-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,hybrid-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,hybrid-green+2))))
   `(gnus-cite-5 ((t (:foreground ,hybrid-green+1))))
   `(gnus-cite-6 ((t (:foreground ,hybrid-green))))
   `(gnus-cite-7 ((t (:foreground ,hybrid-red))))
   `(gnus-cite-8 ((t (:foreground ,hybrid-red-1))))
   `(gnus-cite-9 ((t (:foreground ,hybrid-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,hybrid-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,hybrid-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,hybrid-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,hybrid-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,hybrid-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,hybrid-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,hybrid-bg+2))))
   `(gnus-signature ((t (:foreground ,hybrid-yellow))))
   `(gnus-x ((t (:background ,hybrid-fg :foreground ,hybrid-bg))))
   `(mm-uu-extract ((t (:background ,hybrid-bg-05 :foreground ,hybrid-green+1))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,hybrid-blue))))
   `(guide-key/key-face ((t (:foreground ,hybrid-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,hybrid-green+1))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,hybrid-green
                      :background ,hybrid-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,hybrid-yellow
                      :background ,hybrid-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,hybrid-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,hybrid-bg+1))))
   `(helm-visible-mark ((t (:foreground ,hybrid-bg :background ,hybrid-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,hybrid-green+4 :background ,hybrid-bg-1))))
   `(helm-separator ((t (:foreground ,hybrid-red :background ,hybrid-bg))))
   `(helm-time-zone-current ((t (:foreground ,hybrid-green+2 :background ,hybrid-bg))))
   `(helm-time-zone-home ((t (:foreground ,hybrid-red :background ,hybrid-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,hybrid-orange :background ,hybrid-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,hybrid-magenta :background ,hybrid-bg))))
   `(helm-bookmark-info ((t (:foreground ,hybrid-green+2 :background ,hybrid-bg))))
   `(helm-bookmark-man ((t (:foreground ,hybrid-yellow :background ,hybrid-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,hybrid-magenta :background ,hybrid-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,hybrid-red :background ,hybrid-bg))))
   `(helm-buffer-process ((t (:foreground ,hybrid-cyan :background ,hybrid-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,hybrid-fg :background ,hybrid-bg))))
   `(helm-buffer-size ((t (:foreground ,hybrid-fg-1 :background ,hybrid-bg))))
   `(helm-ff-directory ((t (:foreground ,hybrid-cyan :background ,hybrid-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,hybrid-fg :background ,hybrid-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,hybrid-green+2 :background ,hybrid-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,hybrid-red :background ,hybrid-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,hybrid-yellow :background ,hybrid-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,hybrid-bg :background ,hybrid-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,hybrid-cyan :background ,hybrid-bg))))
   `(helm-grep-file ((t (:foreground ,hybrid-fg :background ,hybrid-bg))))
   `(helm-grep-finish ((t (:foreground ,hybrid-green+2 :background ,hybrid-bg))))
   `(helm-grep-lineno ((t (:foreground ,hybrid-fg-1 :background ,hybrid-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,hybrid-red :background ,hybrid-bg))))
   `(helm-match ((t (:foreground ,hybrid-orange :background ,hybrid-bg-1 :weight bold))))
   `(helm-moccur-buffer ((t (:foreground ,hybrid-cyan :background ,hybrid-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,hybrid-fg-1 :background ,hybrid-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,hybrid-fg :background ,hybrid-bg))))
;;;;; helm-swoop
   `(helm-swoop-target-line-face ((t (:foreground ,hybrid-fg :background ,hybrid-bg+1))))
   `(helm-swoop-target-word-face ((t (:foreground ,hybrid-yellow :background ,hybrid-bg+2 :weight bold))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,hybrid-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,hybrid-bg-05)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,hybrid-bg+1))
                   (t :weight bold)))
;;;;; hydra
   `(hydra-face-red ((t (:foreground ,hybrid-red-1 :background ,hybrid-bg))))
   `(hydra-face-amaranth ((t (:foreground ,hybrid-red-3 :background ,hybrid-bg))))
   `(hydra-face-blue ((t (:foreground ,hybrid-blue :background ,hybrid-bg))))
   `(hydra-face-pink ((t (:foreground ,hybrid-magenta :background ,hybrid-bg))))
   `(hydra-face-teal ((t (:foreground ,hybrid-cyan :background ,hybrid-bg))))
;;;;; info+
   `(info-command-ref-item ((t (:background ,hybrid-bg-1 :foreground ,hybrid-orange))))
   `(info-constant-ref-item ((t (:background ,hybrid-bg-1 :foreground ,hybrid-magenta))))
   `(info-double-quoted-name ((t (:inherit font-lock-comment-face))))
   `(info-file ((t (:background ,hybrid-bg-1 :foreground ,hybrid-yellow))))
   `(info-function-ref-item ((t (:background ,hybrid-bg-1 :inherit font-lock-function-name-face))))
   `(info-macro-ref-item ((t (:background ,hybrid-bg-1 :foreground ,hybrid-yellow))))
   `(info-menu ((t (:foreground ,hybrid-yellow))))
   `(info-quoted-name ((t (:inherit font-lock-constant-face))))
   `(info-reference-item ((t (:background ,hybrid-bg-1))))
   `(info-single-quote ((t (:inherit font-lock-keyword-face))))
   `(info-special-form-ref-item ((t (:background ,hybrid-bg-1 :foreground ,hybrid-yellow))))
   `(info-string ((t (:inherit font-lock-string-face))))
   `(info-syntax-class-item ((t (:background ,hybrid-bg-1 :foreground ,hybrid-blue+1))))
   `(info-user-option-ref-item ((t (:background ,hybrid-bg-1 :foreground ,hybrid-red))))
   `(info-variable-ref-item ((t (:background ,hybrid-bg-1 :foreground ,hybrid-orange))))
;;;;; irfc
   `(irfc-head-name-face ((t (:foreground ,hybrid-red :weight bold))))
   `(irfc-head-number-face ((t (:foreground ,hybrid-red :weight bold))))
   `(irfc-reference-face ((t (:foreground ,hybrid-blue-1 :weight bold))))
   `(irfc-requirement-keyword-face ((t (:inherit font-lock-keyword-face))))
   `(irfc-rfc-link-face ((t (:inherit link))))
   `(irfc-rfc-number-face ((t (:foreground ,hybrid-cyan :weight bold))))
   `(irfc-std-number-face ((t (:foreground ,hybrid-green+4 :weight bold))))
   `(irfc-table-item-face ((t (:foreground ,hybrid-green+3))))
   `(irfc-title-face ((t (:foreground ,hybrid-yellow
                                      :underline t :weight bold))))
;;;;; ivy
   `(ivy-confirm-face ((t (:foreground ,hybrid-green :background ,hybrid-bg))))
   `(ivy-match-required-face ((t (:foreground ,hybrid-red :background ,hybrid-bg))))
   `(ivy-remote ((t (:foreground ,hybrid-blue :background ,hybrid-bg))))
   `(ivy-subdir ((t (:foreground ,hybrid-yellow :background ,hybrid-bg))))
   `(ivy-current-match ((t (:foreground ,hybrid-yellow :weight bold :underline t))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,hybrid-bg+1))))
   `(ivy-minibuffer-match-face-2 ((t (:background ,hybrid-green-1))))
   `(ivy-minibuffer-match-face-3 ((t (:background ,hybrid-green))))
   `(ivy-minibuffer-match-face-4 ((t (:background ,hybrid-green+1))))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,hybrid-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,hybrid-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,hybrid-yellow))))
   `(ido-indicator ((t (:foreground ,hybrid-yellow :background ,hybrid-red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,hybrid-bg+2 :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,hybrid-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,hybrid-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,hybrid-red+1))))
   `(jabber-roster-user-xa ((t (:foreground ,hybrid-magenta))))
   `(jabber-roster-user-chatty ((t (:foreground ,hybrid-orange))))
   `(jabber-roster-user-error ((t (:foreground ,hybrid-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,hybrid-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,hybrid-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,hybrid-red+1))))
   `(jabber-chat-prompt-system ((t (:foreground ,hybrid-green+3))))
   `(jabber-activity-face((t (:foreground ,hybrid-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,hybrid-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,hybrid-orange))))
   `(js2-error ((t (:foreground ,hybrid-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,hybrid-green-1))))
   `(js2-jsdoc-type ((t (:foreground ,hybrid-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,hybrid-green+3))))
   `(js2-function-param ((t (:foreground, hybrid-orange))))
   `(js2-external-variable ((t (:foreground ,hybrid-orange))))
;;;;; additional js2 mode attributes for better syntax highlighting
   `(js2-instance-member ((t (:foreground ,hybrid-green-1))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,hybrid-orange))))
   `(js2-jsdoc-html-tag-name ((t (:foreground ,hybrid-red-1))))
   `(js2-object-property ((t (:foreground ,hybrid-blue+1))))
   `(js2-magic-paren ((t (:foreground ,hybrid-blue-5))))
   `(js2-private-function-call ((t (:foreground ,hybrid-cyan))))
   `(js2-function-call ((t (:foreground ,hybrid-cyan))))
   `(js2-private-member ((t (:foreground ,hybrid-blue-1))))
   `(js2-keywords ((t (:foreground ,hybrid-magenta))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,hybrid-red-1 :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,hybrid-fg :weight normal))))
   `(ledger-font-payee-pending-face ((t (:foreground ,hybrid-red :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,hybrid-bg+1))))
   `(ledger-font-auto-xact-face ((t (:foreground ,hybrid-yellow-1 :weight normal))))
   `(ledger-font-periodic-xact-face ((t (:foreground ,hybrid-green :weight normal))))
   `(ledger-font-pending-face ((t (:foreground ,hybrid-orange weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,hybrid-fg))))
   `(ledger-font-posting-date-face ((t (:foreground ,hybrid-orange :weight normal))))
   `(ledger-font-posting-account-face ((t (:foreground ,hybrid-blue-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,hybrid-fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,hybrid-orange))))
   `(ledger-font-posting-amount-face ((t (:foreground ,hybrid-orange))))
   `(ledger-occur-narrowed-face ((t (:foreground ,hybrid-fg-1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,hybrid-bg+1))))
   `(ledger-font-comment-face ((t (:foreground ,hybrid-green))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,hybrid-red-1 :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,hybrid-fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,hybrid-orange :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,hybrid-orange :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,hybrid-comment :background ,hybrid-bg))))
;;;;; lispy
   `(lispy-command-name-face ((t (:background ,hybrid-bg-05 :inherit font-lock-function-name-face))))
   `(lispy-cursor-face ((t (:foreground ,hybrid-bg :background ,hybrid-fg))))
   `(lispy-face-hint ((t (:inherit highlight :foreground ,hybrid-yellow))))
;;;;; ruler-mode
   `(ruler-mode-column-number ((t (:inherit 'ruler-mode-default :foreground ,hybrid-fg))))
   `(ruler-mode-fill-column ((t (:inherit 'ruler-mode-default :foreground ,hybrid-yellow))))
   `(ruler-mode-goal-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-comment-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-tab-stop ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-current-column ((t (:foreground ,hybrid-yellow :box t))))
   `(ruler-mode-default ((t (:foreground ,hybrid-green+2 :background ,hybrid-bg))))

;;;;; lui
   `(lui-time-stamp-face ((t (:foreground ,hybrid-blue-1))))
   `(lui-hilight-face ((t (:foreground ,hybrid-green+2 :background ,hybrid-bg))))
   `(lui-button-face ((t (:inherit hover-highlight))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,hybrid-green+2 :background ,hybrid-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,hybrid-red+1 :background ,hybrid-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,hybrid-blue+1 :background ,hybrid-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,hybrid-magenta :background ,hybrid-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,hybrid-yellow :background ,hybrid-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
;;;;;; headings and diffs
   `(magit-section-highlight           ((t (:background ,hybrid-bg+05))))
   `(magit-section-heading             ((t (:foreground ,hybrid-yellow :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,hybrid-orange :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,hybrid-bg+05  :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,hybrid-bg+05
                                                        :foreground ,hybrid-orange :weight bold))))
   `(magit-diff-hunk-heading           ((t (:background ,hybrid-bg+1))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,hybrid-bg+2))))
   `(magit-diff-hunk-heading-selection ((t (:background ,hybrid-bg+2
                                                        :foreground ,hybrid-orange))))
   `(magit-diff-lines-heading          ((t (:background ,hybrid-orange
                                                        :foreground ,hybrid-bg+2))))
   `(magit-diff-context-highlight      ((t (:background ,hybrid-bg+05
                                                        :foreground "grey70"))))
   `(magit-diffstat-added   ((t (:foreground ,hybrid-green+4))))
   `(magit-diffstat-removed ((t (:foreground ,hybrid-red))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,hybrid-yellow  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,hybrid-green-1 :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,hybrid-green   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,hybrid-fg-1    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,hybrid-blue-2  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,hybrid-green  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,hybrid-red    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,hybrid-orange))))
   `(magit-log-date      ((t (:foreground ,hybrid-fg-1))))
   `(magit-log-graph     ((t (:foreground ,hybrid-fg+1))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,hybrid-yellow-2))))
   `(magit-sequence-stop ((t (:foreground ,hybrid-green))))
   `(magit-sequence-part ((t (:foreground ,hybrid-yellow))))
   `(magit-sequence-head ((t (:foreground ,hybrid-blue))))
   `(magit-sequence-drop ((t (:foreground ,hybrid-red))))
   `(magit-sequence-done ((t (:foreground ,hybrid-fg-1))))
   `(magit-sequence-onto ((t (:foreground ,hybrid-fg-1))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,hybrid-green))))
   `(magit-bisect-skip ((t (:foreground ,hybrid-yellow))))
   `(magit-bisect-bad  ((t (:foreground ,hybrid-red))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,hybrid-bg-1 :foreground ,hybrid-blue-2))))
   `(magit-blame-hash    ((t (:background ,hybrid-bg-1 :foreground ,hybrid-blue-2))))
   `(magit-blame-name    ((t (:background ,hybrid-bg-1 :foreground ,hybrid-orange))))
   `(magit-blame-date    ((t (:background ,hybrid-bg-1 :foreground ,hybrid-orange))))
   `(magit-blame-summary ((t (:background ,hybrid-bg-1 :foreground ,hybrid-blue-2
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,hybrid-bg+3))))
   `(magit-hash           ((t (:foreground ,hybrid-bg+3))))
   `(magit-tag            ((t (:foreground ,hybrid-orange :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,hybrid-green  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,hybrid-blue   :weight bold))))
   `(magit-branch-current ((t (:foreground ,hybrid-blue   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,hybrid-blue   :weight bold))))
   `(magit-refname        ((t (:background ,hybrid-bg+2 :foreground ,hybrid-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,hybrid-bg+2 :foreground ,hybrid-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,hybrid-bg+2 :foreground ,hybrid-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,hybrid-green))))
   `(magit-signature-bad       ((t (:foreground ,hybrid-red))))
   `(magit-signature-untrusted ((t (:foreground ,hybrid-yellow))))
   `(magit-cherry-unmatched    ((t (:foreground ,hybrid-cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,hybrid-magenta))))
   `(magit-reflog-commit       ((t (:foreground ,hybrid-green))))
   `(magit-reflog-amend        ((t (:foreground ,hybrid-magenta))))
   `(magit-reflog-merge        ((t (:foreground ,hybrid-green))))
   `(magit-reflog-checkout     ((t (:foreground ,hybrid-blue))))
   `(magit-reflog-reset        ((t (:foreground ,hybrid-red))))
   `(magit-reflog-rebase       ((t (:foreground ,hybrid-magenta))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,hybrid-green))))
   `(magit-reflog-remote       ((t (:foreground ,hybrid-cyan))))
   `(magit-reflog-other        ((t (:foreground ,hybrid-cyan))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,hybrid-green+1))))
   `(message-header-other ((t (:foreground ,hybrid-green))))
   `(message-header-to ((t (:foreground ,hybrid-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,hybrid-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,hybrid-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,hybrid-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,hybrid-green))))
   `(message-mml ((t (:foreground ,hybrid-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,hybrid-orange))))
   `(mew-face-header-from ((t (:foreground ,hybrid-yellow))))
   `(mew-face-header-date ((t (:foreground ,hybrid-green))))
   `(mew-face-header-to ((t (:foreground ,hybrid-red))))
   `(mew-face-header-key ((t (:foreground ,hybrid-green))))
   `(mew-face-header-private ((t (:foreground ,hybrid-green))))
   `(mew-face-header-important ((t (:foreground ,hybrid-blue))))
   `(mew-face-header-marginal ((t (:foreground ,hybrid-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,hybrid-red))))
   `(mew-face-header-xmew ((t (:foreground ,hybrid-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,hybrid-red))))
   `(mew-face-body-url ((t (:foreground ,hybrid-orange))))
   `(mew-face-body-comment ((t (:foreground ,hybrid-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,hybrid-green))))
   `(mew-face-body-cite2 ((t (:foreground ,hybrid-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,hybrid-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,hybrid-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,hybrid-red))))
   `(mew-face-mark-review ((t (:foreground ,hybrid-blue))))
   `(mew-face-mark-escape ((t (:foreground ,hybrid-green))))
   `(mew-face-mark-delete ((t (:foreground ,hybrid-red))))
   `(mew-face-mark-unlink ((t (:foreground ,hybrid-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,hybrid-green))))
   `(mew-face-mark-unread ((t (:foreground ,hybrid-red-2))))
   `(mew-face-eof-message ((t (:foreground ,hybrid-green))))
   `(mew-face-eof-part ((t (:foreground ,hybrid-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,hybrid-cyan :background ,hybrid-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,hybrid-bg :background ,hybrid-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,hybrid-bg :background ,hybrid-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,hybrid-blue))))
   `(mingus-pausing-face ((t (:foreground ,hybrid-magenta))))
   `(mingus-playing-face ((t (:foreground ,hybrid-cyan))))
   `(mingus-playlist-face ((t (:foreground ,hybrid-cyan ))))
   `(mingus-mark-face ((t (:bold t :foreground ,hybrid-magenta))))
   `(mingus-song-file-face ((t (:foreground ,hybrid-yellow))))
   `(mingus-artist-face ((t (:foreground ,hybrid-cyan))))
   `(mingus-album-face ((t (:underline t :foreground ,hybrid-red+1))))
   `(mingus-album-stale-face ((t (:foreground ,hybrid-red+1))))
   `(mingus-stopped-face ((t (:foreground ,hybrid-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,hybrid-yellow))))
   `(nav-face-button-num ((t (:foreground ,hybrid-cyan))))
   `(nav-face-dir ((t (:foreground ,hybrid-green))))
   `(nav-face-hdir ((t (:foreground ,hybrid-red))))
   `(nav-face-file ((t (:foreground ,hybrid-fg))))
   `(nav-face-hfile ((t (:foreground ,hybrid-red-4))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,hybrid-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,hybrid-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,hybrid-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,hybrid-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,hybrid-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,hybrid-green-1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,hybrid-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,hybrid-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,hybrid-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,hybrid-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,hybrid-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,hybrid-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,hybrid-bg+1))))
;;;;; neotree
   `(neo-banner-face ((t (:foreground ,hybrid-blue+1 :weight bold))))
   `(neo-header-face ((t (:foreground ,hybrid-fg))))
   `(neo-root-dir-face ((t (:foreground ,hybrid-blue+1 :weight bold))))
   `(neo-dir-link-face ((t (:foreground ,hybrid-blue))))
   `(neo-file-link-face ((t (:foreground ,hybrid-fg))))
   `(neo-expand-btn-face ((t (:foreground ,hybrid-blue))))
   `(neo-vc-default-face ((t (:foreground ,hybrid-fg+1))))
   `(neo-vc-user-face ((t (:foreground ,hybrid-red :slant italic))))
   `(neo-vc-up-to-date-face ((t (:foreground ,hybrid-fg))))
   `(neo-vc-edited-face ((t (:foreground ,hybrid-magenta))))
   `(neo-vc-needs-merge-face ((t (:foreground ,hybrid-red+1))))
   `(neo-vc-unlocked-changes-face ((t (:foreground ,hybrid-red :background ,hybrid-blue-5))))
   `(neo-vc-added-face ((t (:foreground ,hybrid-green+1))))
   `(neo-vc-conflict-face ((t (:foreground ,hybrid-red+1))))
   `(neo-vc-missing-face ((t (:foreground ,hybrid-red+1))))
   `(neo-vc-ignored-face ((t (:foreground ,hybrid-fg-1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,hybrid-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,hybrid-fg :weight bold))))
   `(org-checkbox ((t (:background ,hybrid-bg+2 :foreground ,hybrid-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,hybrid-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,hybrid-red-1))))
   `(org-done ((t (:weight bold :weight bold :foreground ,hybrid-green+3))))
   `(org-formula ((t (:foreground ,hybrid-yellow-2))))
   `(org-headline-done ((t (:foreground ,hybrid-green+3))))
   `(org-hide ((t (:foreground ,hybrid-bg-1))))
   `(org-level-1 ((t (:foreground ,hybrid-orange))))
   `(org-level-2 ((t (:foreground ,hybrid-green+4))))
   `(org-level-3 ((t (:foreground ,hybrid-blue-1))))
   `(org-level-4 ((t (:foreground ,hybrid-yellow-2))))
   `(org-level-5 ((t (:foreground ,hybrid-cyan))))
   `(org-level-6 ((t (:foreground ,hybrid-green+2))))
   `(org-level-7 ((t (:foreground ,hybrid-red-4))))
   `(org-level-8 ((t (:foreground ,hybrid-blue-4))))
   `(org-link ((t (:foreground ,hybrid-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,hybrid-green+4))))
   `(org-scheduled-previously ((t (:foreground ,hybrid-red))))
   `(org-scheduled-today ((t (:foreground ,hybrid-blue+1))))
   `(org-sexp-date ((t (:foreground ,hybrid-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,hybrid-green+2))))
   `(org-tag ((t (:weight bold :weight bold))))
   `(org-time-grid ((t (:foreground ,hybrid-orange))))
   `(org-todo ((t (:weight bold :foreground ,hybrid-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:weight bold :foreground ,hybrid-red :weight bold :underline nil))))
   `(org-column ((t (:background ,hybrid-bg-1))))
   `(org-column-title ((t (:background ,hybrid-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,hybrid-fg :background ,hybrid-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,hybrid-bg :background ,hybrid-red-1))))
   `(org-ellipsis ((t (:foreground ,hybrid-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,hybrid-cyan :underline t))))
   `(org-document-title ((t (:foreground ,hybrid-blue))))
   `(org-document-info ((t (:foreground ,hybrid-blue))))
   `(org-habit-ready-face ((t :background ,hybrid-green)))
   `(org-habit-alert-face ((t :background ,hybrid-yellow-1 :foreground ,hybrid-bg)))
   `(org-habit-clear-face ((t :background ,hybrid-blue-3)))
   `(org-habit-overdue-face ((t :background ,hybrid-red-3)))
   `(org-habit-clear-future-face ((t :background ,hybrid-blue-4)))
   `(org-habit-ready-future-face ((t :background ,hybrid-green-1)))
   `(org-habit-alert-future-face ((t :background ,hybrid-yellow-2 :foreground ,hybrid-bg)))
   `(org-habit-overdue-future-face ((t :background ,hybrid-red-4)))
;;;;; outline
   `(outline-1 ((t (:foreground ,hybrid-orange))))
   `(outline-2 ((t (:foreground ,hybrid-green+4))))
   `(outline-3 ((t (:foreground ,hybrid-blue-1))))
   `(outline-4 ((t (:foreground ,hybrid-yellow-2))))
   `(outline-5 ((t (:foreground ,hybrid-cyan))))
   `(outline-6 ((t (:foreground ,hybrid-green+2))))
   `(outline-7 ((t (:foreground ,hybrid-red-4))))
   `(outline-8 ((t (:foreground ,hybrid-blue-4))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,hybrid-yellow-2 :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,hybrid-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,hybrid-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,hybrid-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,hybrid-bg+3 :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,hybrid-fg :background ,hybrid-bg+2))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,hybrid-bg :background ,hybrid-orange))))
   `(proof-error-face ((t (:foreground ,hybrid-fg :background ,hybrid-red-4))))
   `(proof-highlight-dependency-face ((t (:foreground ,hybrid-bg :background ,hybrid-yellow-1))))
   `(proof-highlight-dependent-face ((t (:foreground ,hybrid-bg :background ,hybrid-orange))))
   `(proof-locked-face ((t (:background ,hybrid-blue-5))))
   `(proof-mouse-highlight-face ((t (:foreground ,hybrid-bg :background ,hybrid-orange))))
   `(proof-queue-face ((t (:background ,hybrid-red-4))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,hybrid-red-2))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,hybrid-bg))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,hybrid-bg))))
   `(proof-warning-face ((t (:foreground ,hybrid-bg :background ,hybrid-yellow-1))))
;;;;; racket-mode
   `(racket-keyword-argument-face ((t (:inherit font-lock-constant-face))))
   `(racket-selfeval-face ((t (:inherit font-lock-type-face))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,hybrid-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,hybrid-green+4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,hybrid-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,hybrid-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,hybrid-green+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,hybrid-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,hybrid-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,hybrid-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,hybrid-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,hybrid-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,hybrid-green))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,hybrid-blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,hybrid-blue))))
   `(rcirc-other-nick ((t (:foreground ,hybrid-orange))))
   `(rcirc-bright-nick ((t (:foreground ,hybrid-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,hybrid-blue-2))))
   `(rcirc-server ((t (:foreground ,hybrid-green))))
   `(rcirc-server-prefix ((t (:foreground ,hybrid-green+1))))
   `(rcirc-timestamp ((t (:foreground ,hybrid-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,hybrid-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:weight bold))))
   `(rcirc-prompt ((t (:foreground ,hybrid-yellow :weight bold))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:weight bold))))
   `(rcirc-url ((t (:weight bold))))
   `(rcirc-keyword ((t (:foreground ,hybrid-yellow :weight bold))))
;;;;; re-builder
   `(reb-match-0 ((t (:foreground ,hybrid-bg :background ,hybrid-magenta))))
   `(reb-match-1 ((t (:foreground ,hybrid-bg :background ,hybrid-blue))))
   `(reb-match-2 ((t (:foreground ,hybrid-bg :background ,hybrid-orange))))
   `(reb-match-3 ((t (:foreground ,hybrid-bg :background ,hybrid-red))))
;;;;; regex-tool
   `(regex-tool-matched-face ((t (:background ,hybrid-blue-4 :weight bold))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,hybrid-green))))
   `(rpm-spec-doc-face ((t (:foreground ,hybrid-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,hybrid-red))))
   `(rpm-spec-macro-face ((t (:foreground ,hybrid-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,hybrid-red))))
   `(rpm-spec-package-face ((t (:foreground ,hybrid-red))))
   `(rpm-spec-section-face ((t (:foreground ,hybrid-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,hybrid-blue))))
   `(rpm-spec-var-face ((t (:foreground ,hybrid-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,hybrid-orange))))
   `(rst-level-2-face ((t (:foreground ,hybrid-green+1))))
   `(rst-level-3-face ((t (:foreground ,hybrid-blue-1))))
   `(rst-level-4-face ((t (:foreground ,hybrid-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,hybrid-cyan))))
   `(rst-level-6-face ((t (:foreground ,hybrid-green-1))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,hybrid-yellow :weight bold))))
   `(sh-quoted-exec ((t (:foreground ,hybrid-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,hybrid-red+1 :background ,hybrid-bg+3 :weight bold))))
   `(show-paren-match ((t (:background ,hybrid-bg+3 :weight bold))))
;;;;; smart-mode-line
   ;; use (setq sml/theme nil) to enable Hybrid for sml
   `(sml/global ((,class (:foreground ,hybrid-fg :weight bold))))
   `(sml/modes ((,class (:foreground ,hybrid-yellow :weight bold))))
   `(sml/minor-modes ((,class (:foreground ,hybrid-fg-1 :weight bold))))
   `(sml/filename ((,class (:foreground ,hybrid-yellow :weight bold))))
   `(sml/line-number ((,class (:foreground ,hybrid-blue :weight bold))))
   `(sml/col-number ((,class (:foreground ,hybrid-blue+1 :weight bold))))
   `(sml/position-percentage ((,class (:foreground ,hybrid-blue-1 :weight bold))))
   `(sml/prefix ((,class (:foreground ,hybrid-orange))))
   `(sml/git ((,class (:foreground ,hybrid-green+3))))
   `(sml/process ((,class (:weight bold))))
   `(sml/sudo ((,class  (:foreground ,hybrid-orange :weight bold))))
   `(sml/read-only ((,class (:foreground ,hybrid-red-2))))
   `(sml/outside-modified ((,class (:foreground ,hybrid-orange))))
   `(sml/modified ((,class (:foreground ,hybrid-red))))
   `(sml/vc-edited ((,class (:foreground ,hybrid-green+2))))
   `(sml/charging ((,class (:foreground ,hybrid-green+4))))
   `(sml/discharging ((,class (:foreground ,hybrid-red+1))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,hybrid-red+1 :background ,hybrid-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,hybrid-bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,hybrid-red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,hybrid-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hybrid-red)))
      (t
       (:underline ,hybrid-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hybrid-orange)))
      (t
       (:underline ,hybrid-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hybrid-yellow)))
      (t
       (:underline ,hybrid-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hybrid-green)))
      (t
       (:underline ,hybrid-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,hybrid-green+2))))
   `(speedbar-directory-face ((t (:foreground ,hybrid-cyan))))
   `(speedbar-file-face ((t (:foreground ,hybrid-fg))))
   `(speedbar-highlight-face ((t (:foreground ,hybrid-bg :background ,hybrid-green+2))))
   `(speedbar-selected-face ((t (:foreground ,hybrid-red))))
   `(speedbar-separator-face ((t (:foreground ,hybrid-bg :background ,hybrid-blue-1))))
   `(speedbar-tag-face ((t (:foreground ,hybrid-yellow))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,hybrid-fg
                                    :background ,hybrid-bg))))
   `(tabbar-selected ((t (:foreground ,hybrid-fg
                                      :background ,hybrid-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,hybrid-fg
                                        :background ,hybrid-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,hybrid-bg
                                       :background ,hybrid-bg-1))))
   `(term-color-red ((t (:foreground ,hybrid-red-2
                                     :background ,hybrid-red-4))))
   `(term-color-green ((t (:foreground ,hybrid-green
                                       :background ,hybrid-green+2))))
   `(term-color-yellow ((t (:foreground ,hybrid-orange
                                        :background ,hybrid-yellow))))
   `(term-color-blue ((t (:foreground ,hybrid-blue-1
                                      :background ,hybrid-blue-4))))
   `(term-color-magenta ((t (:foreground ,hybrid-magenta
                                         :background ,hybrid-red))))
   `(term-color-cyan ((t (:foreground ,hybrid-cyan
                                      :background ,hybrid-blue))))
   `(term-color-white ((t (:foreground ,hybrid-fg
                                       :background ,hybrid-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,hybrid-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,hybrid-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,hybrid-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,hybrid-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,hybrid-cyan))))
;;;;; visual-regexp
   `(vr/group-0 ((t (:foreground ,hybrid-bg :background ,hybrid-green :weight bold))))
   `(vr/group-1 ((t (:foreground ,hybrid-bg :background ,hybrid-orange :weight bold))))
   `(vr/group-2 ((t (:foreground ,hybrid-bg :background ,hybrid-blue :weight bold))))
   `(vr/match-0 ((t (:inherit isearch))))
   `(vr/match-1 ((t (:foreground ,hybrid-yellow-2 :background ,hybrid-bg-1 :weight bold))))
   `(vr/match-separator-face ((t (:foreground ,hybrid-red :weight bold))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,hybrid-bg-05))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,hybrid-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,hybrid-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,hybrid-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,hybrid-blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,hybrid-blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,hybrid-orange))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,hybrid-cyan))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,hybrid-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,hybrid-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,hybrid-bg+1 :foreground ,hybrid-bg+1))))
   `(whitespace-hspace ((t (:background ,hybrid-bg+1 :foreground ,hybrid-bg+1))))
   `(whitespace-tab ((t (:background ,hybrid-red-1))))
   `(whitespace-newline ((t (:foreground ,hybrid-bg+1))))
   `(whitespace-trailing ((t (:background ,hybrid-red))))
   `(whitespace-line ((t (:background ,hybrid-bg :foreground ,hybrid-magenta))))
   `(whitespace-space-before-tab ((t (:background ,hybrid-orange :foreground ,hybrid-orange))))
   `(whitespace-indentation ((t (:background ,hybrid-yellow :foreground ,hybrid-red))))
   `(whitespace-empty ((t (:background ,hybrid-yellow))))
   `(whitespace-space-after-tab ((t (:background ,hybrid-yellow :foreground ,hybrid-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,hybrid-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,hybrid-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,hybrid-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,hybrid-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,hybrid-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,hybrid-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,hybrid-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,hybrid-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,hybrid-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,hybrid-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,hybrid-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,hybrid-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,hybrid-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,hybrid-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,hybrid-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,hybrid-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,hybrid-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,hybrid-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,hybrid-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,hybrid-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,hybrid-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,hybrid-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,hybrid-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,hybrid-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,hybrid-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,hybrid-green+4))))
;;;;; xcscope
   `(cscope-file-face ((t (:foreground ,hybrid-yellow :weight bold))))
   `(cscope-function-face ((t (:foreground ,hybrid-cyan :weight bold))))
   `(cscope-line-number-face ((t (:foreground ,hybrid-red :weight bold))))
   `(cscope-mouse-face ((t (:foreground ,hybrid-bg :background ,hybrid-blue+1))))
   `(cscope-separator-face ((t (:foreground ,hybrid-red :weight bold
                                            :underline t :overline t))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,hybrid-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,hybrid-bg-1 :foreground ,hybrid-bg-1))))
   ))

;;; Theme Variables
(hybrid-with-color-variables
  (custom-theme-set-variables
   'hybrid
;;;;; ansi-color
   `(ansi-color-names-vector [,hybrid-bg ,hybrid-red ,hybrid-green ,hybrid-yellow
                                          ,hybrid-blue ,hybrid-magenta ,hybrid-cyan ,hybrid-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,hybrid-bg-05)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,hybrid-red ,hybrid-orange ,hybrid-yellow ,hybrid-green ,hybrid-green+4
                    ,hybrid-cyan ,hybrid-blue+1 ,hybrid-magenta))
;;;;; pdf-tools
   `(pdf-view-midnight-colors '(,hybrid-fg . ,hybrid-bg-05))
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,hybrid-red-1)
       ( 40. . ,hybrid-red)
       ( 60. . ,hybrid-orange)
       ( 80. . ,hybrid-yellow-2)
       (100. . ,hybrid-yellow-1)
       (120. . ,hybrid-yellow)
       (140. . ,hybrid-green-1)
       (160. . ,hybrid-green)
       (180. . ,hybrid-green+1)
       (200. . ,hybrid-green+2)
       (220. . ,hybrid-green+3)
       (240. . ,hybrid-green+4)
       (260. . ,hybrid-cyan)
       (280. . ,hybrid-blue-2)
       (300. . ,hybrid-blue-1)
       (320. . ,hybrid-blue)
       (340. . ,hybrid-blue+1)
       (360. . ,hybrid-magenta)))
   `(vc-annotate-very-old-color ,hybrid-magenta)
   `(vc-annotate-background ,hybrid-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar hybrid-add-font-lock-keywords nil
  "Whether to add font-lock keywords for hybrid color names.
In buffers visiting library `hybrid-theme.el' the hybrid
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar hybrid-colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after hybrid activate)
;;   "Maybe also add font-lock keywords for hybrid colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or hybrid-add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "hybrid-theme.el")))
;;     (unless hybrid-colors-font-lock-keywords
;;       (setq hybrid-colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car hybrid-colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc hybrid-colors-alist))))))
;;     (font-lock-add-keywords nil hybrid-colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after hybrid activate)
;;   "Also remove font-lock keywords for hybrid colors."
;;   (font-lock-remove-keywords nil hybrid-colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'hybrid)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; hybrid-theme.el ends here
