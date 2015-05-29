(prelude-require-packages '(bundler rspec-mode rvm))

;;use rvm
(require 'rvm)
(rvm-use-default)

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
 '(rspec-use-rake-when-possible nil)
 ;;;;
 ;; # When using rspec snippets:
 ;;
 ;; describe do
 ;;   it "is awesome" { true }
 ;; end
 ;;
 ;; # instead of
 ;;
 ;; RSpec.describe do
 ;;  RSpec.before do
 ;;    self.gag_with_four_fingers
 ;;  end
 ;;  RSpec.it "is so terrible why would you" { true }
 ;; end
 ;;
 '(rspec-expose-dsl-globally t)
 ;; Turn off insane utf-8 comment insertion in Ruby files
 '(ruby-insert-encoding-magic-comment nil))
