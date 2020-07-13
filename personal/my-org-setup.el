;; First, ensure that the latest version of org mode is installed
;; from https://github.com/bbatsov/prelude/issues/1155#issuecomment-377322880

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Force org install from latest mainline branch
(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(use-package org :ensure org-plus-contrib :pin org)


;; Now do personal org mode setup
(defun my-org-setup ()
  "Rameshwar's org mode stuff in addition to prelude"
  (setq-default org-hide-emphasis-markers t
                org-startup-indented t
                truncate-lines nil
                word-wrap t
                linum-mode t)
  (org-num-mode)
  (org-indent-mode))

(add-hook 'org-mode-hook 'my-org-setup)
