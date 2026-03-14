# Prelude Haskell

!!! Note

    This module builds on top of the shared [Programming](programming.md) module.

## Haskell Mode

This module bundles [haskell-mode](https://github.com/haskell/haskell-mode) and
enables several useful minor modes:

- **subword-mode** - CamelCase aware editing
- **eldoc-mode** - display type signatures in the echo area
- **haskell-indentation-mode** - intelligent indentation for
  Haskell code
- **interactive-haskell-mode** - interaction with a GHCi process

## Getting Started

Make sure you have GHC and Cabal (or Stack) installed on your system. Run
<kbd>C-h m</kbd> in a Haskell buffer to see all available key
bindings and features.

## LSP Alternative

For a more feature-rich experience, you might want to use
[Haskell Language Server (HLS)](https://github.com/haskell/haskell-language-server)
together with the [LSP](lsp.md) module. You'll need to set
this up yourself in your
personal config.
