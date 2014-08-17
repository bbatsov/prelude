;;; Code:
;; Setup ERC
(setq erc-autojoin-channels-alist '(("freenode.net"
                                     "#emacs" "#clojure")))
;; Regular IRC
(setq erc-nick "gastove")
(when (file-exists-p (expand-file-name "~/.ercpass"))
  (load "~/.ercpass")
  (require 'erc-services)
  (erc-services-mode 1)
  (setq erc-prompt-for-nickserv-password nil)
  (setq erc-nickserv-passwords
        `((freenode ((,erc-nick . ,erc-pass))))))

(provide 'erc-configs)

;;; erc-configs.el ends here
