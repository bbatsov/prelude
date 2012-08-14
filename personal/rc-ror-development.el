;; setup rinari package
(add-to-list 'load-path "~/.emacs.d/vendor/rinari")
(require 'rinari)

(unless (package-installed-p 'mmm-mode)
  (package-refresh-contents)
  (package-install 'mmm-mode))

(require 'mmm-mode)
