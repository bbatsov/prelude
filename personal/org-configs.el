;;; org-configs.el -- configs to extend prelude's built-in ERC support

;;; Commentary:
;; This package provides configuration values to augment prelude's
;; org-mode support.  You should store passwords in an encrypted file
;; ~/passwords.org.gpg

;;; Code:
(prelude-require-packages '(org org-password-manager))

(require 'org)
(require 'org-agenda)
(require 'org-password-manager)

(add-hook 'org-mode-hook 'org-password-manager-key-bindings)

(add-to-list 'org-agenda-files "~/passwords.org.gpg")

(provide 'org-configs)
;;; org-configs.el ends here
