(require 'package)
(setq package-enable-at-startup nil)
(mapc (lambda (source) (add-to-list 'package-archives source) t)
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
                                        ;("org" . "https://orgmode.org/elpa/")
        ))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package treemacs
  :ensure t
  :defer t)
