(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(custom-safe-themes
   (quote
    ("fc6e906a0e6ead5747ab2e7c5838166f7350b958d82e410257aeeb2820e8a07a" "1f3304214265481c56341bcee387ef1abb684e4efbccebca0e120be7b1a13589" default)))
 '(fci-rule-color "#383838")
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

;; Pull in custom packages
(prelude-require-packages '(ahg jade-mode php-mode twilight-theme ess twittering-mode floobits))

;; Load my Theme of Choice
(load-theme 'twilight t)

;; Disable whitespace-mode in certain other major modes
(add-hook 'php-mode-hook (lambda() (whitespace-mode -1)))

;; Load ESS
(require 'ess-site)

;; Use IPython as my python interpreter
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; Tweak Mac Keyboard Behavior
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; Configure Twittering Mode
(setq twittering-icon-mode t)
(setq twittering-use-master-password t)
(setq twittering-use-icon-storage t)

;; Setup ERC
(setq erc-autojoin-channels-alist '(("freenode.net"
                                     "#emacs" "#clojure" "#julia")))

(setq erc-nick "gastove")
(when (file-exists-p (expand-file-name "~/.ercpass"))
  (load "~/.ercpass")
  (require 'erc-services)
  (erc-services-mode 1)
  (setq erc-prompt-for-nickserv-password nil)
  (setq erc-nickserv-passwords
        '((freenode ((erc-nick . ,erc-pass))))))

;; For SQL hackery -- needs more work
;; Generate a list of DBs I connect to commonly
(setq sql-connection-alist
      '((sag-db01
         (sql-product 'mysql)
         (sql-server "sag-db01.sea.bigfishgames.com")
         (sql-user (getenv "MYSQL_USER"))
         (sql-password (getenv "MYSQL_PASSWORD"))
         (sql-database "wordace")
         (sql-port 3306))
        (character-batch
         (sql-product 'mysql)
         (sql-server "characterbatch-dbslave.sea.bigfishgames.com")
         (sql-user (getenv "MYSQL_USER"))
         (sql-password (getenv "MYSQL_PASSWORD"))
         (sql-database "")
         (sql-port 3306))))

(defun sql-connect-preset (name)
  "Connect to a predefined SQL connection listed in `sql-connection-alist'"
  (eval `(let ,(cdr (assoc name sql-connection-alist))
    (flet ((sql-get-login (&rest what)))
      (sql-product-interactive sql-product)))))

;; Function to load a DB based on its short name
(defun sql-connect-preset-by-name (name)
  "Connect to a DB by entering it's short name"
  (interactive "sDB Name: ")
  (sql-connect-preset 'name))

(defun sql-sag-db01 ()
  (interactive)
  (sql-connect-preset 'sag-db01))

(global-set-key (kbd "M-s q") 'sql-connect-preset-by-name) ; Connect to a db preset by nameq

(provide 'custom)

;;; custom.el ends here
