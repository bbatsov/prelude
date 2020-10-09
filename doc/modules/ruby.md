# Prelude Ruby

!!! Note

    This module builds on top of the shared [Programming](programming.md) module.

## Ruby Mode

Emacs comes with Ruby programming support through the built-in
`ruby-mode`. Whenever you are editing Ruby code run `C-h m` to
look at the Python mode key bindings. Alternatively look at the
menu bar entries under Ruby. To toggle the menu bar press `F12`.

Prelude enables `CamelCase` aware editing in Ruby code (via `subword-mode`).

## inf-ruby

The module bundles the [inf-ruby](https://github.com/nonsequitur/inf-ruby) package which allows you to run a Ruby
REPL (e.g. `irb` or `pry`) in an Emacs buffer and interact with it from
Ruby source buffers.

## yari

The module bundles the [yari](https://github.com/hron/yari.el) package which allows you to search in Ruby's RI
documentation. Use `C-h R` to invoke it.
