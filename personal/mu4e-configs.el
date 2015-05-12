;;; mu4e-configs.el --- My mu4e configs
;;
;;; Commentary:
;; Borrowed, gratefully, from http://kirang.in/2014/11/13/emacs-as-email-client-with-offlineimap-and-mu4e-on-osx/
;;; Code:

;; If I want multiple accounts, need to manage these:
;; (setq mu4e-sent-folder "/Account1/Saved Items" ;; check
;;      mu4e-drafts-folder "/Account1/Drafts"     ;; check
;;      user-mail-address "my.address@account1.tld" ;; check
;;      smtpmail-default-smtp-server "smtp.account1.tld" ;; check
;;      smtpmail-local-domain "account1.tld"
;;      smtpmail-smtp-server "smtp.account1.tld" ;; check
;;      smtpmail-stream-type starttls
;;      smtpmail-smtp-service 25)

;; Re-enable C-x m for email (nerfs eshell, which I never use)
(global-set-key (kbd "C-x m") 'compose-mail)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(setq mu4e-maildir "~/.Mail")
(setq mu4e-drafts-folder "/gastove@gmail.com/[Gmail].Drafts")
(setq mu4e-sent-folder   "/gastove@gmail.com/[Gmail].Sent Mail")
;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)
;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; Make mu4e the default user agent
(setq mail-user-agent 'mu4e-user-agent)

;; mu4e mail dirs
(require 'mu4e-maildirs-extension)
(mu4e-maildirs-extension)

;; shortcuts
(setq mu4e-maildir-shortcuts
      '(("/gastove@gmail.com/INBOX"               . ?i)
        ("/gastove@gmail.com/[Gmail].Important"   . ?I)
        ("/gastove@gmail.com/[Gmail].Sent Mail"   . ?s)))

;; something about ourselves
(setq
 user-mail-address "gastove@gmail.com"
 user-full-name  "Ross Donaldson"
 mu4e-compose-signature
 (concat
  "Cheers,\n"
  "Ross\n"))

;; ISO date format
(setq mu4e-headers-date-format "%Y-%m-%d")

;; show images
(setq mu4e-show-images t)

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; convert html emails properly
;; Possible options:
;;   - html2text -utf8 -width 72
;;   - textutil -stdin -format html -convert txt -stdout
;;   - html2markdown | grep -v '&nbsp_place_holder;' (Requires html2text pypi)
;;   - w3m -dump -cols 80 -T text/html
;;   - view in browser (provided below)
(setq mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")

;; spell check
;; (add-hook 'mu4e-compose-mode-hook
;;           (defun my-do-compose-stuff ()
;;             "My settings for message composition."
;;             ;;(set-fill-column 80)
;;             ;; (flyspell-mode)
;;             ))

;; add option to view html message in a browser
;; `aV` in view to activate
(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; fetch mail every 10 mins
(setq mu4e-update-interval 600)


;; configuration for sending mail
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; Tweak bookmarked querries
(add-to-list 'mu4e-bookmarks '("flag:unread AND date:today..now" "Today's Unreads" ?h))

;; Switch accounts

(defvar my-mu4e-account-alist
  '(("gastove@gmail.com"
     (mu4e-drafts-folder "/gastove@gmail.com/[Gmail].Drafts")
     (mu4e-sent-folder   "/gastove@gmail.com/[Gmail].Sent Mail")
     (user-mail-address "gastove@gmail.com"))
    ("ross@urbanairship.com"
     (mu4e-drafts-folder "/ross@urbanairship.com/[Gmail].Drafts")
     (mu4e-sent-folder   "/ross@urbanairship.com/[Gmail].Sent Mail")
     (user-mail-address "ross@urbanairship.com"))))

(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

(provide 'mu4e-configs)

;;; mu4e-configs.el ends here
