# Modules

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

To use a module you simply have to require it. No new concepts. No magic.

## Foundation Modules

These modules provide shared functionality used by other modules:

- [Programming](programming.md) - common foundation for all programming modes
- [Lisp Base](lisp.md) - common foundation for Lisp-family language modules
- [LSP](lsp.md) - common foundation for modules using the Language Server Protocol
- [Company](company.md) - completion framework used across many modes

## Programming Language Modules

The following programming languages have enhanced support in Prelude:

- [C/C++](c.md)
- [Clojure](clojure.md)
- [CoffeeScript](coffee.md)
- [Common Lisp](common_lisp.md)
- [CSS](css.md)
- [Dart](dart.md)
- [Elixir](elixir.md)
- [Emacs Lisp](emacs_lisp.md)
- [Erlang](erlang.md)
- [F#](fsharp.md)
- [Go](go.md)
- [Haskell](haskell.md)
- [JavaScript](js.md)
- [LaTeX](latex.md)
- [Lua](lua.md)
- [OCaml](ocaml.md)
- [Perl](perl.md)
- [Python](python.md)
- [Racket](racket.md)
- [Ruby](ruby.md)
- [Rust](rust.md)
- [Scala](scala.md)
- [Scheme](scheme.md)
- [SCSS](scss.md)
- [Shell](shell.md)
- [TypeScript](ts.md)
- [Web/HTML](web.md)
- [XML](xml.md)
- [YAML](yaml.md)

## Completion Frameworks

Prelude supports several completion frameworks. You should
only enable **one** of these:

- [Helm](helm.md) - incremental completion and narrowing
- [Ido](ido.md) - built-in completion with fuzzy matching
- [Ivy](ivy.md) - lightweight completion with Swiper/Counsel
- [Vertico](vertico.md) - vertical completion with Consult

## Other Modules

- [ERC](erc.md) - IRC client configuration
- [Evil](evil.md) - Vim emulation
- [Key Chord](key_chord.md) - simultaneous key press bindings
- [Literate Programming](literate-programming.md) - org-babel
  and Jupyter notebook support
- [Org Mode](orgmode.md) - org-mode configuration
