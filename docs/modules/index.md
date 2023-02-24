# Modules

!!! Note

    Most modules are not currently documented. Helping out with their
    documentation is a great way to contribute to the project!

Prelude provides extra functionality through modules. Some modules may
require extra steps to enable all functionality. These steps and the
functionality provided by these modules are documented on the
following links.

## What's a module?

Prelude modules are plain old Elisp libraries - there's absolutely
nothing magical about them.  Most of them simply install a few Emacs
packages and provide some sensible baseline configuration for them.
Here's a real example.

``` emacs-lisp
;;; prelude-ruby.el --- Emacs Prelude: A nice setup for Ruby (and Rails) devs.
;;
;;; Code:

(require 'prelude-programming)

(prelude-require-packages '(inf-ruby yari))

;; We never want to edit Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

;; Map yari to C-h R
(define-key 'help-command (kbd "R") 'yari)

(with-eval-after-load 'ruby-mode
  (defun prelude-ruby-mode-defaults ()
    ;; Don't auto-insert encoding comments
    ;; Those are almost never needed in Ruby 2+
    (setq ruby-insert-encoding-magic-comment nil)
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

## Programming Language Modules

The following programming languages have enhanced support in Prelude:

- C/C++
- [Clojure](clojure.md)
- CoffeeScript
- [Common Lisp](common_lisp.md)
- CSS
- [Dart](dart.md)
- Elixir
- [Emacs Lisp](emacs_lisp.md)
- Erlang
- Go
- Haskell
- JavaScript
- LaTeX
- [Lisp Base](lisp.md) (common foundation for Lisp modules)
- LSP (common foundation for all modules relying on `lsp-mode`)
- Lua
- Markdown
- OCaml
- Perl
- [Programming Base](programming.md) (common foundation for programming modules)
- [Python](python.md)
- Racket
- [Ruby](ruby.md)
- Rust
- Scala
- [Scheme](scheme.md)
- SCSS
- Shell
- TypeScript
- Web
- XML
- YAML

## Other Modules

- [Company](company.md)
- [ERC](erc.md)
- evil
- helm
- ido
- ivy
- key-chord
- Org Mode
- selectrum
- vertico
