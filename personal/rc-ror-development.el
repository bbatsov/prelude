;; setup rinari package
(add-to-list 'load-path "~/.emacs.d/vendor/rinari")
(require 'rinari)

(unless (package-installed-p 'mmm-mode)
  (package-refresh-contents)
  (package-install 'mmm-mode))

(require 'mmm-mode)
(setq mmm-global-mode 'auto)

;; configure html editing
(mmm-add-mode-ext-class 'html-erb-mode nil 'html-js)
(mmm-add-mode-ext-class 'html-erb-mode nil 'html-css)
(mmm-add-mode-ext-class 'html-erb-mode "\\.html\\.erb\\'" 'erb)
(mmm-add-mode-ext-class 'html-erb-mode "\\.jst\\.ejs\\'" 'ejs)

(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . html-erb-mode))
(add-to-list 'auto-mode-alist '("\\.jst\\.ejs\\'" . html-erb-mode))
