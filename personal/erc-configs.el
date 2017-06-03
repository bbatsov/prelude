;;; erc-configs.el -- configs to extend prelude's built-in ERC support

;;; Commentary:
;; This package provides configuration values to allow ERC to
;; authenticate and connect with IRC networks over TLS.

;;; Code:
(require 'erc)
(require 'erc-services)

(erc-services-mode 1)

(setq erc-prompt-for-password t
      erc-prompt-for-nickserv-password t
      erc-default-nicks "jcgrillo"
      erc-default-port 6697
      erc-autojoin-channels-alist '(("freenode.net"
                                     "#clojure"
                                     "#clojure-web"
                                     "#clojure-beginners"
                                     "#python"
                                     "#emacs"
                                     "#bostonpython"
                                     "##java"
                                     "##rust")
                                    ("mozilla.org"
                                     "#rust")))

(provide 'erc-configs)
;;; erc-configs.el ends here
