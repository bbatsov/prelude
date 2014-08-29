(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(custom-safe-themes (quote ("fc6e906a0e6ead5747ab2e7c5838166f7350b958d82e410257aeeb2820e8a07a" "1f3304214265481c56341bcee387ef1abb684e4efbccebca0e120be7b1a13589" default)))
 '(fci-rule-color "#383838")
 '(safe-local-variable-values (quote ((project-venv-name . "mashboard"))))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Pull in custom packages
(prelude-require-packages '(ahg
                            jade-mode
                            twilight-theme
                            ess
                            twittering-mode
                            floobits
                            sublime-themes
                            company
                            virtualenvwrapper
                            org
                            protobuf-mode
                            jedi
                            ensime
                            web-mode))

;; Pull in all my personal bits and bobs from external files
(defvar load-personal-config-list)
(setq load-personal-config-list '("/jsl-checker.el"
                                  "/jsx-configs.el"
                                  "/work.el"            ;; Contains work erc configs too.
                                  "/python-configs.el"
                                  "/erc-configs.el"
                                  "/jsx-configs.el"
                                  "/web-mode-configs.el"))

(mapc (lambda (rmd-file-name)
        (load (concat prelude-personal-dir rmd-file-name)))
      load-personal-config-list)

;;; Color Theme
(load-theme 'junio t)

;;; Whitespace Mode
;; Disable whitespace-mode in certain other major modes
(add-hook 'php-mode-hook (lambda() (whitespace-mode -1)))
(add-hook 'org-mode-hook (lambda() (whitespace-mode -1)))
(add-hook 'markdown-mode-hook (lambda () (whitespace-mode -1)))

;;; ESS:
;; Load ESS
(require 'ess-site)

;;; Tweak Mac Keyboard Behavior
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;;; Projectile
(setq projectile-remember-window-configs t)
(setq projectile-switch-project-action 'projectile-dired)
(global-set-key (kbd "s-p") 'projectile-switch-projec)

;;; Twittering-mode
(setq twittering-icon-mode t)
(setq twittering-use-master-password t)
(setq twittering-use-icon-storage t)

;; Company-mode everywhere
(add-hook 'global-init-hook 'global-company-mode)

;; Smartparens all the time
(add-hook 'global-init-hook 'smartparens-mode)

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

;;; Org Mode
;; Support for Babel Mode code blocks
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp .t)
   (clojure . t)))

;; Config org export backends
(setq org-export-backends
      `(beamer
        ascii
        markdown
        deck))

;; Hide org emphasis marks
(setq org-hide-emphasis-markers t)

;;; Scala
;; Ensime
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(provide 'custom)

;;; custom.el ends here
