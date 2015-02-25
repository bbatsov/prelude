(prelude-require-packages '(bundler rspec-mode))

;; rspec-mode makes you explicitly require snippets nowadays
(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))

(add-hook 'ruby-mode-hook 'prelude-enable-whitespace)

(custom-set-variables
 ;; don't indent like crazy.
 '(ruby-deep-arglist nil)
 ;; who uses rake to run rspec. honestly
 '(rspec-use-rake-when-possible nil))
