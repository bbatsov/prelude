;;; work.el -- All my work configs in one place.

;;; Code:
;; Work IRC
(defun start-irc-work ()
  "Connect to the UA IRC Server"
  (interactive)
  (add-to-list 'erc-autojoin-channels-alist '("dev.urbanairship.com" "#ops"))
  ;; (load "~/.ercpass")
  (erc-tls :server "dev.urbanairship.com"
           :port 6697
           :nick erc-nick
))

;;; Work SQL Functions
;; Generate a list of DBs I connect to commonly
(when (file-exists-p (expand-file-name "~/.emacs-dbs"))
  (load "~/.emacs-dbs")
  (setq sql-connection-alist
        '((yavin
           (sql-product 'postgres)
           (sql-server yavin-server)
           (sql-user yavin-user)
           (sql-password yavin-password)
           (sql-database "yavin")))))

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

(defun sql-yavin ()
  (interactive)
  (sql-connect-preset 'yavin))

(global-set-key (kbd "M-s q") 'sql-connect-preset-by-name) ; Connect to a db preset by nameq

(provide 'work)

;;; work.el ends here
