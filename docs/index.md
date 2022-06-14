# Emacs Prelude

Prelude is an Emacs distribution that aims to enhance the default
Emacs experience.  Prelude alters a lot of the default settings,
bundles a plethora of additional packages and adds its own core
library to the mix. The final product offers an easy to use Emacs
configuration for Emacs newcomers and lots of additional power for
Emacs power users.

!!! Important

    Prelude is compatible **ONLY with GNU Emacs 25.1+**. In general you're
    advised to always run Prelude with the latest stable Emacs release.

You can support the development of Prelude via
[GitHub Sponsors](https://github.com/sponsors/bbatsov),
[ko-fi](https://www.ko-fi.com/bbatsov),
[PayPal](https://www.paypal.me/bbatsov) and
[Patreon](https://www.patreon.com/bbatsov).

## Features

* Improved UX, that's still in line with Emacs traditions
* Sane defaults of baseline Emacs functionality
* Automatic installation of many major programming modes on demand
* A curated set of 3rd party packages to enhance the base functionality
* Simple modular architecture
* Easy customization

## Package Highlights

Here are some of the essential 3rd party packages that Prelude adds to Emacs:

* [ace-window](https://github.com/abo-abo/ace-window)
  (effective navigation between multiple windows)
* [avy](https://github.com/abo-abo/avy)
  (effective navigation)
* [crux](https://github.com/bbatsov/crux)
  (lots of useful editing commands)
* [diff-hl](https://github.com/dgutov/diff-hl)
  (shows colorful diff markers in the gutter when you're editing files
  under version control)
* [easy-kill](https://github.com/leoliu/easy-kill)
* [editorconfig-mode](https://github.com/editorconfig/editorconfig-emacs)
  (teaches Emacs to respect [.editorconfig](https://editorconfig.org/))
* [expand-region](https://github.com/magnars/expand-region.el)
* [flycheck](https://www.flycheck.org/)
  (modern integration with many lint tools)
* [guru-mode](https://github.com/bbatsov/guru-mode)
  (an Emacs guru that helps you learn basic Emacs keybindings)
* [projectile](https://github.com/bbatsov/projectile)
  (powerful project navigation/interaction package)
* [magit](https://magit.vc/)
  (the best git client in the known universe)
* [git-timemachine](https://gitlab.com/pidu/git-timemachine)
  (navigate quickly through different versions of one file)
* `nlinum`
  (line numbers in your buffers)
* [smartparens](https://github.com/Fuco1/smartparens)
  (powerful package for dealing with expressions and matched
  delimiters in programming languages)
* [super-save](https://github.com/bbatsov/super-save)
  (auto-save buffers when moving around)
* [which-key](https://github.com/justbur/emacs-which-key)
  (shows you possible keybindings when you type a partial keybinding)
* [zenburn-theme](https://github.com/bbatsov/zenburn-emacs)
  (Prelude's default color theme)
* [undo-tree](https://elpa.gnu.org/packages/undo-tree.html)
  (A powerful way to navigate your editing history)

On top of this Prelude bundles a bunch of smaller packages and makes
many more packages available via optional modules.

## Programming Languages Support

The following programming languages have enhanced support in Prelude:

- C/C++
- [Clojure](modules/clojure.md)
- CoffeeScript
- [Common Lisp](modules/common_lisp.md)
- CSS
- [Dart](modules/dart.md)
- [Emacs Lisp](modules/emacs_lisp.md)
- Erlang
- Elixir
- [F#](modules/fsharp.md)
- Go
- Haskell
- JavaScript
- LaTeX
- [Lisp Base](modules/lisp.md) (common foundation for Lisp modules)
- Lua
- Markdown
- [OCaml](modules/ocaml.md)
- Org Mode
- Perl
- [Python](modules/python.md)
- Racket
- [Ruby](modules/ruby.md)
- [Rust](modules/rust.md)
- Scala
- [Scheme](modules/scheme.md)
- SCSS
- TypeScript
- HTML (via `web-mode`)
- XML
- YAML

On top of this - basic support for many other programming languages
will be auto-installed when needed (e.g. the first time you open a
source file for some language).

## Philosophy

Prelude's philosophy is quite simple:

* simple
* easy to understand and extend
* stable
* a foundation for you to build upon, as opposed to some end-user product

This means that it intentionally doesn't pack all the bells and
whistles that it could.  Prelude aims to enhance the classic Emacs
experience without deviating a lot from it - e.g.  it would never
enable something like `evil-mode` (vim keybindings) by default and so
on.

All the third-party packages that it bundles are carefully vetted and
are known to be of good quality and to have reliable maintainers. That
generally means that Prelude's unlikely to immediately adopt some
shiny new package, that has established tried and true alternatives.

In practice this translates to the following:

* Prelude is less opinionated than distros like Spacemacs and Doom
  Emacs (meaning it's closer to the standard Emacs experience)
* Prelude installs relatively few additional packages by default
* Most modules in Prelude are opt-in instead of opt-out (you'll notice
  the default config enables only a handful of modules)
* Most modules (for example, modules for programming languages) are
  pretty short and feature setup only for essential packages (in some
  cases that would be just the major mode for the language in
  question)
* You don't really need to track Prelude's upstream - you're
  encouraged to just fork it and use it as the basis for your own
  configuration.

Remember that the ultimate goal of every Emacs user is to create an
Emacs setup that reflects their own experience, needs, goals and
ideas. Just like Lisp, Emacs is nothing but a raw building material
for the perfect editing experience.
