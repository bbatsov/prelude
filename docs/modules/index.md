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
;;; prelude-ruby.el --- Emacs Prelude: A nice setup for Ruby devs.
;;; Code:

(require 'prelude-programming)

(prelude-require-packages '(inf-ruby yari))

;; Use ruby-ts-mode when the tree-sitter grammar is available
(when (treesit-ready-p 'ruby t)
  (add-to-list 'major-mode-remap-alist
               '(ruby-mode . ruby-ts-mode)))

;; Map yari to C-h R
(define-key 'help-command (kbd "R") 'yari)

(defun prelude-ruby-mode-defaults ()
  (setq ruby-insert-encoding-magic-comment nil)
  (inf-ruby-minor-mode +1)
  (subword-mode +1)
  (prelude-lsp-enable))

(setq prelude-ruby-mode-hook 'prelude-ruby-mode-defaults)

(add-hook 'ruby-mode-hook (lambda ()
                            (run-hooks 'prelude-ruby-mode-hook)))
(add-hook 'ruby-ts-mode-hook (lambda ()
                               (run-hooks 'prelude-ruby-mode-hook)))

(provide 'prelude-ruby)
;;; prelude-ruby.el ends here
```

To use a module you simply have to require it. No new
concepts. No magic.

## Writing a Module

When writing or modifying a module, follow these conventions
for consistency:

### Structure

A typical programming language module follows this pattern:

``` emacs-lisp
;;; prelude-example.el --- Emacs Prelude: Example config.
;;; Code:

(require 'prelude-programming)

(prelude-require-packages '(example-mode))

;; Use tree-sitter mode when grammar is available
(when (treesit-ready-p 'example t)
  (add-to-list 'major-mode-remap-alist
               '(example-mode . example-ts-mode)))

(defun prelude-example-mode-defaults ()
  (subword-mode +1)
  (prelude-lsp-enable))

(setq prelude-example-mode-hook
      'prelude-example-mode-defaults)

(add-hook 'example-mode-hook
          (lambda ()
            (run-hooks 'prelude-example-mode-hook)))
(add-hook 'example-ts-mode-hook
          (lambda ()
            (run-hooks 'prelude-example-mode-hook)))

(provide 'prelude-example)
;;; prelude-example.el ends here
```

### Conventions

- **Require `prelude-programming`** as the foundation for
  all programming language modules. Lisp-family modules
  should require `prelude-lisp` instead.
- **Define a `prelude-*-mode-defaults` function** with the
  mode-specific setup. This makes it easy for users to
  override.
- **Use the `prelude-*-mode-hook` variable pattern** (setq +
  add-hook with lambda). This lets users replace the
  defaults function entirely via their personal config.
- **Enable `subword-mode`** for CamelCase-aware editing.
- **Call `prelude-lsp-enable`** for LSP support. This
  respects the user's `prelude-lsp-client` setting
  (Eglot or lsp-mode).
- **Add tree-sitter support** using `treesit-ready-p` with
  the `t` argument (silent, no error if grammar missing)
  and `major-mode-remap-alist`. Always add hooks for both
  the legacy mode and the tree-sitter mode.
- **Use `with-eval-after-load`** to defer configuration
  until the relevant package is loaded.
- **Install packages with `prelude-require-packages`**,
  not `use-package` or manual `package-install` calls.

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
