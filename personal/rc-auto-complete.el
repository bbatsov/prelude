(unless (package-installed-p 'auto-complete)
  (package-refresh-contents)
  (package-install 'auto-complete))

(require 'auto-complete-config)
(ac-config-default)
