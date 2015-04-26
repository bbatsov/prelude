(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(custom-safe-themes
   (quote
    ("9122dfb203945f6e84b0de66d11a97de6c9edf28b3b5db772472e4beccc6b3c5" "ad9fc392386f4859d28fe4ef3803585b51557838dbc072762117adad37e83585" "132ccc75b7fdcd9f5979329620a1151953a8f65efad06b988deed7cba9338eab" "fc6e906a0e6ead5747ab2e7c5838166f7350b958d82e410257aeeb2820e8a07a" "1f3304214265481c56341bcee387ef1abb684e4efbccebca0e120be7b1a13589" default)))
 '(fci-rule-color "#383838")
 '(org-agenda-files (quote ("~/Code/astromech/notes.org")))
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
 '(company-scrollbar-bg ((t (:background "#303030"))))
 '(company-scrollbar-fg ((t (:background "#232323"))))
 '(company-tooltip ((t (:inherit default :background "#1c1c1c"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(helm-ff-directory ((t (:background "gray0" :foreground "DarkRed"))))
 '(helm-selection ((t (:background "gray14"))))
 '(helm-source-header ((t (:background "DarkOrange4" :foreground "white" :weight bold :height 1.3 :family "Sans Serif"))))
 '(jabber-chat-prompt-foreign ((t (:foreground "steel blue" :weight bold))))
 '(jabber-chat-prompt-local ((t (:foreground "light gray" :weight bold)))))

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
                            column-enforce-mode
                            smart-mode-line))

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
                                  "/web-mode-configs.el"))

(mapc (lambda (rmd-file-name)
        (load (concat prelude-personal-dir rmd-file-name)))
      load-personal-config-list)

;; PCRE Regexes
(rxt-global-mode)

;;; Smart Mode Line
(sml/setup)

;; Magit warnings OFF
(setq magit-last-seen-setup-instructions "1.4.0")

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


;; COLORS
(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; Not convinced this is helping.
(setq company-idle-delay .4)
;; (setq company-minimum-prefix-length 1)
(setq company-tooltip-limit 20)

;;; Go Configs
;; Totally re-enable these if you ever do Go again.
;; (let ((go-path (getenv "GOPATH")))
;;   (load (concat go-path "/src/code.google.com/p/go.tools/cmd/oracle/oracle.el")))

;; Smartparens all the time
(smartparens-global-mode t)
(sp-local-pair 'org-mode "~" "~")
(sp-local-pair 'org-mode "/" "/")
(sp-local-pair 'org-mode "*" "*")

;;; Support for Marked.app -- assumes you're on a Mac,
;;; and have Marked.app installed.
(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "open -a /Applications/Marked.app %s"
           (shell-quote-argument (buffer-file-name))))
  )
(global-set-key (kbd "C-c m") 'markdown-preview-file)

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
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;;; Polymode for markdown
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . poly-markdown-mode))

;;; A handy-dandy function for rotating windows
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)

(provide 'custom)

;;; custom.el ends here
