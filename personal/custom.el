(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/savefile/bookmarks")
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c0f286b90603c8309d69e79f1fa038fd1f00670e1e09bd9d014ed1adc51f7261" "9122dfb203945f6e84b0de66d11a97de6c9edf28b3b5db772472e4beccc6b3c5" "ad9fc392386f4859d28fe4ef3803585b51557838dbc072762117adad37e83585" "132ccc75b7fdcd9f5979329620a1151953a8f65efad06b988deed7cba9338eab" "fc6e906a0e6ead5747ab2e7c5838166f7350b958d82e410257aeeb2820e8a07a" "1f3304214265481c56341bcee387ef1abb684e4efbccebca0e120be7b1a13589" default)))
 '(fci-rule-color "#383838")
 '(org-agenda-files
   (quote
    ("~/Dropbox/org-docs/cotidienne.org" "~/Code/astromech/notes.org")))
 '(safe-local-variable-values (quote ((project-venv-name . "mashboard"))))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; Pull in custom packages
(prelude-require-packages '(jade-mode
                            ess
                            twittering-mode
                            floobits
                            sublime-themes
                            company
                            company-jedi
                            virtualenvwrapper
                            org
                            protobuf-mode
                            jedi
                            ensime
                            web-mode
                            thrift
                            ag
                            jabber
                            malabar-mode
                            org-plus-contrib
                            polymode
                            badger-theme
                            helm-ag
                            wgrep
                            wgrep-ag
                            pcre2el
                            wgrep-helm
                            clj-refactor
                            perspective
                            persp-projectile
                            column-enforce-mode
                            smart-mode-line
                            rich-minority
                            mu4e-maildirs-extension
                            bookmark+
                            sauron
                            ggtags))

;; Pull in all my personal bits and bobs from external files
(defvar load-personal-config-list)
(setq load-personal-config-list '("/jsl-checker.el"
                                  ;;"/evil.el"      ;; Disable for now, fucks w/ cider
                                  "/jsx-configs.el"
                                  "/work.el"            ;; Contains work erc configs too.
                                  "/python-configs.el"
                                  "/erc-configs.el"
                                  "/ess-configs.el"
                                  "/jsx-configs.el"
                                  "/jabber-configs.el"
                                  "/web-mode-configs.el"
                                  "/flip-tables.el"
                                  "/custom-fns.el"
                                  "/mu4e-configs.el"))

(mapc (lambda (rmd-file-name)
        (load (concat prelude-personal-dir rmd-file-name)))
      load-personal-config-list)

;; PCRE Regexes
(rxt-global-mode)

;;; Smart Mode Line and rich-minority-mode
(sml/setup)
(which-function-mode -1)
(rich-minority-mode 1)
(setq rm-blacklist '(" MRev"
                     " yas"
                     " Helm"
                     " company"
                     " guru"
                     " Pre"))

(setq sml/shorten-directory t)
(setq sml/shorten-modes t)

;; Java and scala package names are infinite and terrible; shorten them.
(add-to-list 'sml/replacer-regexp-list '("^~/Code/" ":CODE:") t)
(add-to-list 'sml/replacer-regexp-list '("^:CODE:\\(?:.*\\)\\{1,2\\}/src/main/java/" ":SMJ:") t)
(add-to-list 'sml/replacer-regexp-list '("^:CODE:\\(?:.*\\)\\{1,2\\}/src/test/java/" ":STJ:") t)
(add-to-list 'sml/replacer-regexp-list '("^:CODE:\\(?:.*\\)\\{1,2\\}/src/main/scala/" ":SMS:") t)
(add-to-list 'sml/replacer-regexp-list '("^:CODE:\\(?:.*\\)\\{1,2\\}/src/test/scala/" ":STS:") t)
(add-to-list 'sml/replacer-regexp-list '("^:SM[JS]:com/urbanairship/\\(.*\\)/" ":M:\\1:") t)
(add-to-list 'sml/replacer-regexp-list '("^:ST[JS]:com/urbanairship/\\(.*\\)/" ":T:\\1:") t)

;; Make sure I notice when I'm in
(add-to-list 'rm-text-properties '(" Sp/s" 'face 'font-lock-warning-face))

;;; Magit warnings OFF
(setq magit-last-seen-setup-instructions "1.4.0")

;;; wgrep-ag support
(autoload 'wgrep-ag-setup "wgrep-ag")
(add-hook 'ag-mode-hook 'wgrep-ag-setup)
(add-hook 'helm-ag-mode-hook 'wgrep-ag-setup)

;;; Helm
(setq helm-split-window-in-side-p t
      helm-split-window-default-side 'below)

;;; Whitespace and Auto-Fill
;; Set auto-fill to 80 characters by default instead of 70
(setq-default fill-column 80)

;; Disable whitespace-mode and enable auto-fill in prose-writing major modes
(defun text-settings ()
  (whitespace-mode -1)
  (abbrev-mode -1)
  (turn-on-auto-fill))

;; Don't clean up whitespace in markdown mode only
(add-hook 'markdown-mode-hook
          (lambda ()
            (make-local-variable 'prelude-clean-whitespace-on-save)
            (setq-local prelude-clean-whitespace-on-save nil)))

(add-hook 'org-mode-hook 'text-settings)
(add-hook 'markdown-mode-hook 'text-settings)
(add-hook 'rst-mode-hook 'text-settings)

;;; Tweak Mac Keyboard Behavior
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;;; Projectile
(setq projectile-remember-window-configs t)
(setq projectile-switch-project-action 'projectile-dired)
(persp-mode)
(require 'persp-projectile)
(define-key projectile-mode-map (kbd "C-c p p") 'projectile-persp-switch-project)
;;; Twittering-mode
(setq twittering-icon-mode t)
(setq twittering-use-master-password t)
(setq twittering-use-icon-storage t)

;;; Company-mode
;; errwhrr
(add-hook 'global-init-hook 'global-company-mode)
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))

;; Not convinced this is helping.
(setq company-idle-delay .4)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-limit 20)

;;; Go Configs
;; Totally re-enable these if you ever do Go again.
;; (let ((go-path (getenv "GOPATH")))
;;   (load (concat go-path "/src/code.google.com/p/go.tools/cmd/oracle/oracle.el")))

;; Smartparens all the time
(smartparens-global-mode t)

;;; yasnippet
(yas-global-mode 1)

;; Tell yas to use system autocomplete instead of an f'ed-up X window:
(setq yas-prompt-functions '(yas-completing-prompt))

;;; Malabar Mode for the Jabbas
;; (require 'cedet)
;; (require 'semantic)
;; (load "semantic/loaddefs.el")
;; (semantic-mode 1);;
;; (require 'malabar-mode)
;; (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))

;;; Clojure
;; Enable refactoring support
(require 'clj-refactor)
(add-hook 'clojure-mode-hook
          (lambda ()
            (clj-refactor-mode 1)
            (add-hook 'cider-connected-hook #'cljr-update-artifact-cache)
            (add-hook 'cider-connected-hook #'cljr-warm-ast-cache)
            (cljr-add-keybindings-with-prefix "s-r")))

;;; Scala
;; Ensime
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;;; Polymode for markdown
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . poly-markdown-mode))

;;----------------------------------Bookmark+-----------------------------------
(require 'bookmark+)

;;-----------------------------------Sauron-------------------------------------
(require 'sauron)
;; 1: On OSX, there's no dbus, so don't try to load it.
;; 2: On Ubuntu, there _is_ dbus, so use it to get mu new mail updates from cron
(if (eq system-type 'darwin)
    (setq sauron-modules '(sauron-jabber sauron-erc sauron-org sauron-twittering sauron-notifications))
  (setq sauron-dbus-cookie 1))

(setq sauron-separate-frame nil
      sauron-nick-insensitivity 300)

(add-hook 'sauron-event-block-functions
          (lambda (origin prio msg &optional props)
            (and
             (eq 'twittering origin)
             (string-match "^[[:digit:]]* new tweets" msg))))

;;----------------------------------Timezones-----------------------------------
(setq display-time-world-list
      '(("America/Los_Angeles" "Pacific")
        ("America/Denver" "Mountain")
        ("America/Mexico_City" "Central")
        ("America/New_York" "Eastern")
        ("Atlantic/Reykjavik" "Iceland")
        ("Europe/Paris" "Paris, France")))

(provide 'custom)

;;; custom.el ends here
