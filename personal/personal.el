;;; abizern.el --- Emacs Prelude: Personal settings
;;
;;; Commentary:
;; Personal settings to augment those of Prelude

;;; Code:

;; Turn on auto-fill in text modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)

;; Set Menlo 12 as my default font
(set-frame-font "-apple-Menlo-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1")
(modify-frame-parameters nil '((wait-for-wm . nil)))

;; Set up what my default browser is
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; Load my paths so that eshell knows what I can do
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Make sure there is a final newline
(setq-default require-final-newline t)

;; Get rid of trailing white space on saves
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Turn off long line highlighting.
(setq whitespace-line-column 250)

;; But it would be nice to autoflow long paragraphs
(setq-default fill-column 80)

;; Turn off whitespace mode
(setq prelude-whitespace nil)

;;; Set up for working with Lisp
(setq slime-default-lisp 'clisp)

;;; Set up for working with Ruby

;; Set file modes
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))

;; Bind RET to newline-and-indent (bored of usig C-j all the time)
(add-hook 'ruby-mode-hook '(lambda ()
                             (local-set-key (kbd "RET") 'newline-and-indent)))

;; Keybindings

;;; I like to use shell, not eshell
(global-set-key (kbd "C-x m") 'shell)

;;; This overrides the keybinding from Prelude-osx.el
(global-set-key (kbd "C-c C-w") 'backward-kill-word)

;;; org-mode
;;;; Keybinding for org-agenda
(define-key global-map (kbd "C-c a") 'org-agenda)
;;;; Don't keep track of when a TODO item is finished
(setq org-log-done nil)

;;; recentf
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-c C-r") 'recentf-open-files)

;;; Movement by line
;;;; Remaps goto-line so that line numbers are turned on only when needed.
(global-set-key [remap goto-line] 'goto-line-with-feedback)

;;; Join the next line to this one
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;;; Move more quickly
;;;; move about in steps of 5 with C-S insteard of just C-
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-line -5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))

;; YASnippet

;;; Remove binding of <tab> key from Markdown cycle
;;; This way I can use YASnippets in my markdown files.
(defun markdown-unset-tab ()
  "markdown-mode-hook"
  (define-key markdown-mode-map (kbd "<tab>") nil))
(add-hook 'markdown-mode-hook '(lambda() (markdown-unset-tab)))

;;; Use ido-mode for choosing snippets
(setq yas/prompt-functions '(yas/ido-prompt yas/dropdown-prompt))

;; Turn off Guru-mode
(defun disable-guru-mode ()
  (guru-mode -1)
)

(add-hook 'prelude-prog-mode-hook 'disable-guru-mode t)

;; ghc-mod
(defvar ghc-mod-dir (concat prelude-vendor-dir "ghc-mod/"))
(add-to-list 'load-path ghc-mod-dir)
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;; Enable erase-buffer
(put 'erase-buffer 'disabled nil)

;; I like to use 4 spaces instead of tabs
(setq c-default-style "bsd"
      c-basic-offset 4)

(setq indent-tabs-mode nil)

(setq c-mode-hook
      (function (lambda ()
                  (setq indent-tabs-mode nil)
                  (setq c-indent-level 4))))
(setq objc-mode-hook
      (function (lambda ()
                  (setq indent-tabs-mode nil)
                  (setq c-indent-level 4))))
(setq c++-mode-hook
      (function (lambda ()
                  (setq indent-tabs-mode nil)
                  (setq c-indent-level 4))))

;; Tell gist to use curl
(setq gist-use-curl t)

;; Give me back my command key
(setq mac-command-modifier 'super)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Let's try Solarized as the theme
;;(load-theme 'solarized-dark t)

;; Make sure the packages I like are installed
(prelude-ensure-module-deps '(haskell-mode markdown-mode solarized-theme sass-mode textile-mode inf-ruby windresize ace-jump-mode exec-path-from-shell))

;; Turn off flyspell - it gets in the way more than it helps
;;(setq prelude-flyspell nil)

;; Cursor settings
(setq-default cursor-type 'bar)
(set-cursor-color "gold1")

;; If I'm running emacs, then I want it to be a server
(require 'server)
(unless (server-running-p)
  (server-start))
