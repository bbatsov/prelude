# Modules

!!! Note

    Most modules are not currently documented. Helping out with their
    documentation is a great way to contribute to the project!

Prelude provides extra functionality through modules. Some modules may
require extra steps to enable all functionality. These steps and the
functionality provided by these modules are documented on the
following links.

## What's a module?

Prelude modules are plain old Elisp libraries - there's absolutely nothing magical about them.
Most of them simply install a few package and provide some sensible baseline configuration for them.
Here's a real example.

``` emacs-lisp
;;; prelude-ruby.el --- Emacs Prelude: A nice setup for Ruby (and Rails) devs.
;;
;;; Code:

(require 'prelude-programming)

(prelude-require-packages '(inf-ruby yari))

;; We never want to edit Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

(define-key 'help-command (kbd "R") 'yari)

(with-eval-after-load 'ruby-mode
  (defun prelude-ruby-mode-defaults ()
    (inf-ruby-minor-mode +1)
    ;; CamelCase aware editing operations
    (subword-mode +1))

  (setq prelude-ruby-mode-hook 'prelude-ruby-mode-defaults)

  (add-hook 'ruby-mode-hook (lambda ()
                              (run-hooks 'prelude-ruby-mode-hook))))

(provide 'prelude-ruby)
;;; prelude-ruby.el ends here
```

To use a module you simple have to require it. No new concepts. No magic.

## List of Modules

- C/C++
- Clojure
- CoffeeScript
- Common Lisp
- CSS
- Emacs-Lisp
- [ERC](erc.md)
- Erlang
- Elixir
- Haskell
- JavaScript
- LaTeX
- Lisp Base
- Markdown
- Org Mode
- Perl
- [Python](python.md)
- Ruby
- Rust
- Scala
- Scheme
- SCSS
- Web
