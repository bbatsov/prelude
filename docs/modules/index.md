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

;; Use ruby-ts-mode when the tree-sitter grammar is available
(prelude-treesit-remap 'ruby 'ruby-mode 'ruby-ts-mode)

;; We never want to edit Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

(defun prelude-ruby-mode-defaults ()
  ;; Don't auto-insert encoding comments
  ;; Those are almost never needed in Ruby 2+
  (setq ruby-insert-encoding-magic-comment nil)
  (inf-ruby-minor-mode +1)
  ;; CamelCase aware editing operations
  (subword-mode +1)
  (prelude-lsp-enable))

;; Run a Ruby REPL (IRB/Pry) inside Emacs and send code to it
(use-package inf-ruby
  :ensure t
  :defer t)

;; Browse Ruby documentation via ri (C-h R)
(use-package yari
  :ensure t
  :defer t
  :bind (:map help-map ("R" . yari)))

(add-hook 'prelude-ruby-mode-hook #'prelude-ruby-mode-defaults)

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

;; Use the tree-sitter mode when its grammar is available
(prelude-treesit-remap 'example 'example-mode 'example-ts-mode)

(use-package example-mode
  :ensure t
  :defer t)

(defun prelude-example-mode-defaults ()
  (subword-mode +1)
  (prelude-lsp-enable))

(add-hook 'prelude-example-mode-hook #'prelude-example-mode-defaults)

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
- **Use the `prelude-*-mode-hook` pattern**: add the
  defaults function to the hook with `add-hook`, and run the
  hook from the major mode hooks. Users can then add to it,
  or remove the defaults function, from their personal config.
- **Enable `subword-mode`** for CamelCase-aware editing.
- **Call `prelude-lsp-enable`** for LSP support. This
  respects the user's `prelude-lsp-client` setting
  (Eglot or lsp-mode).
- **Add tree-sitter support** with `prelude-treesit-remap`,
  which switches to the tree-sitter mode when its grammar is
  available (and offers to install it otherwise). When there's
  no classic mode to remap, map file names directly with
  `prelude-treesit-auto-mode`. Always add hooks for both the
  legacy mode and the tree-sitter mode.
- **Use `with-eval-after-load`** to defer configuration
  until the relevant package is loaded.
- **Install packages with `use-package` and `:ensure t`**,
  not manual `package-install` calls. Prelude keeps track of
  the packages installed this way (see `prelude-packages`).

## Design Notes

Most modules are tiny, but a few of the choices behind them aren't
obvious from the code.

### Which major mode a module uses

Modules prefer the tree-sitter mode that comes with Emacs, as long as
it needs a single grammar that Prelude can install for you. When the
grammar is missing (and you don't want it installed) you get the
classic mode instead.

<!-- markdownlint-disable MD013 -->

| Approach | Modules |
| -------- | ------- |
| Built-in tree-sitter mode, classic mode as the fallback | C/C++, C#, CSS, Go, Java, JavaScript (and JSON), Python, Ruby, Shell |
| Tree-sitter mode only, as Emacs has no classic mode for the language | Rust, Swift, TypeScript, OCaml |
| Tree-sitter mode, with a package installed on demand as the fallback | Elixir, YAML |
| Classic mode | everything else |

<!-- markdownlint-enable MD013 -->

A few modules stick with the classic mode even though a tree-sitter
mode exists:

- **Clojure** - `clojure-ts-mode` needs Emacs 30 and CIDER still relies
  on `clojure-mode` here and there.
- **Kotlin** - `kotlin-ts-mode` needs Emacs 30 and works only with the
  unreleased version of the Kotlin grammar.
- **PHP** - `php-ts-mode` needs Emacs 30 and half a dozen grammars.

The pages of those modules show how to opt into the tree-sitter mode.

Go is a bit special: `go-ts-mode` does the editing, but `go-mode` is
still installed, as that's where `gofmt` and `godoc` live.

### Tree-sitter grammars

Grammars are installed the first time you open a file that needs one,
not at startup, and by default Prelude asks before it does that (see
`prelude-treesit-auto-install`). Building a grammar takes a C compiler
and a moment of your time, so it's not something to do for languages
you might never touch.

Prelude pins the version of every grammar it installs
(`prelude-treesit-language-sources`). Grammars change their node names
from time to time, and a tree-sitter mode written against an older
grammar loses most of its highlighting when that happens, so the
latest version is rarely the one you want. For the same reason the pin
can differ between Emacs versions: `csharp-ts-mode` in Emacs 29 needs a
much older `c-sharp` grammar than the one in Emacs 30.

Emacs 31 knows where to get the grammars of its own modes. Prelude's
recipes are used only when Emacs has none.

### LSP

Modules call `prelude-lsp-enable`, which starts Eglot or lsp-mode
depending on `prelude-lsp-client`. A few things follow from that:

- Prelude doesn't install language servers. That's a job for your
  system's package manager (or the language's own). The exceptions are
  languages where the server isn't a simple executable and needs a
  helper package to install and start it: Java (`eglot-java` or
  `lsp-java`), Julia (`eglot-jl` or `lsp-julia`) and F#
  (`eglot-fsharp`).
- Those helper packages are specific to an LSP client, and which one
  gets installed is decided when the module is loaded. That's why
  `prelude-lsp-client` has to be set in `personal/preload/` - by the
  time your regular personal config is loaded it's too late.
- When the server for a language isn't installed, Prelude doesn't
  start Eglot. Left to its own devices Eglot would complain about the
  missing server for every file you open, while Prelude mentions it
  once for a major mode and you carry on without LSP. The SQL module
  doesn't even do that, as people open `.sql` files all the time
  without having any use for a language server there.
- The Lisp modules (Clojure, Common Lisp, Emacs Lisp, Racket, Scheme)
  don't start a language server at all. They are built around a REPL
  connection (CIDER, SLIME, Geiser, etc.), which does the same job and
  then some.

### cc-mode and its children

The [C/C++](c.md) module configures `c-mode-common-hook`, which runs for
every mode built on top of `cc-mode`, not just for C and C++. The
classic `java-mode` and `csharp-mode` are such modes, and so is
`php-mode`, so they also get the C module's indentation settings
when it's enabled. The tree-sitter modes have nothing to do with
`cc-mode` and aren't affected.

## Foundation Modules

These modules provide shared functionality used by other modules:

- [Programming](programming.md) - common foundation for all programming modes
- [Lisp Base](lisp.md) - common foundation for Lisp-family language modules
- [LSP](lsp.md) - common foundation for modules using the Language Server Protocol
- [Eglot Booster](eglot-booster.md) - speeds up Eglot via
  the `emacs-lsp-booster` wrapper
- [Company](company.md) - completion framework (legacy)
- [Corfu](corfu.md) - lightweight modern alternative to Company
- [Apheleia](apheleia.md) - async, unified format-on-save

## Programming Language Modules

The following programming languages have enhanced support in Prelude:

- [C/C++](c.md)
- [C#](csharp.md)
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
- [Java](java.md)
- [JavaScript](js.md)
- [Julia](julia.md)
- [Kotlin](kotlin.md)
- [LaTeX](latex.md)
- [Lua](lua.md)
- [Nix](nix.md)
- [OCaml](ocaml.md)
- [Perl](perl.md)
- [PHP](php.md)
- [Python](python.md)
- [R](r.md)
- [Racket](racket.md)
- [Ruby](ruby.md)
- [Rust](rust.md)
- [Scala](scala.md)
- [Scheme](scheme.md)
- [SCSS](scss.md)
- [Shell](shell.md)
- [SQL](sql.md)
- [Terraform](terraform.md)
- [TypeScript](ts.md)
- [Web/HTML](web.md)
- [XML](xml.md)
- [YAML](yaml.md)
- [Zig](zig.md)

## Completion Frameworks

Prelude supports several completion frameworks. You should
only enable **one** of these:

- [Helm](helm.md) - incremental completion and narrowing
- [Ido](ido.md) - built-in completion with fuzzy matching
- [Ivy](ivy.md) - lightweight completion with Swiper/Counsel
- [Vertico](vertico.md) - vertical completion with Consult

## Other Modules

- [AI Assistant](ai.md) - LLM-backed chat via gptel
- [ERC](erc.md) - IRC client configuration
- [Evil](evil.md) - Vim emulation
- [Forge](forge.md) - GitHub/GitLab/Gitea PRs and issues via Magit
- [Key Chord](key_chord.md) - simultaneous key press bindings
- [Literate Programming](literate-programming.md) - org-babel
  and Jupyter notebook support
- [Org Mode](orgmode.md) - org-mode configuration
