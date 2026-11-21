# Prelude Ruby

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## Ruby Mode

The module uses the tree-sitter powered `ruby-ts-mode` that comes with
Emacs. Prelude offers to install the `ruby` grammar the first time you
open a Ruby file (see `prelude-treesit-auto-install`). Without the
grammar you get the classic `ruby-mode`.

Whenever you are editing Ruby code run `C-h m` to look at the Ruby
mode key bindings. Alternatively look at the menu bar entries under
Ruby. To toggle the menu bar press `F12`.

Prelude enables `CamelCase` aware editing in Ruby code (via `subword-mode`).

## inf-ruby

The module bundles the
[inf-ruby](https://github.com/nonsequitur/inf-ruby)
package which allows you to run a Ruby
REPL (e.g. `irb` or `pry`) in an Emacs buffer and interact with it from
Ruby source buffers.

## yari

The module bundles the
[yari](https://github.com/hron/yari.el) package which
allows you to search in Ruby's RI
documentation. Use `C-h R` to invoke it.

## LSP support

The module starts a language server when there's one installed. Eglot
looks for [Solargraph](https://solargraph.org/) (`solargraph`) and
[Ruby LSP](https://shopify.github.io/ruby-lsp/) (`ruby-lsp`), in that
order. See the [LSP](lsp.md) module for the available LSP key
bindings.
