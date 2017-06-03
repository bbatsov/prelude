;;; pass-configs.el -- Configuration for pass-store.
;;
;;; Commentary:
;;
;; This package provides configurations for working with pass-store.

;;; Code:
(prelude-require-packages '(simpleclip))
(load-file "~/.emacs.d/personal/pass-store.el")
(require 'pass-store)

(setq pass-store-password-length 48
      pass-store-password-symbols t)

(provide 'pass-configs)
;;; pass-configs.el ends here
