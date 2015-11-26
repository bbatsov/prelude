;;; erc-configs.el -- configs to extend prelude's built-in ERC support

;;; Commentary:
;; This package provides configuration values and functions to allow
;; ERC to authenticate and connect with IRC networks over TLS.

;;; Code:
(setq erc-prompt-for-password nil
      erc-default-nicks "jcgrillo")

(defun freenode-connect ()
  "Connect to freenode.irc.net using TLS."
  (irc-connect )) ;; TODO: implement

(defun irc-connect ()
  "Connect to an IRC server using TLS."
  ) ;; TODO: implement

(provide 'erc-configs)
;;; erc-configs.el ends here
