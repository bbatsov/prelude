# Prelude Haskell

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

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

## LSP support

The module starts a language server when there's one installed. Both
Eglot and lsp-mode use the
[Haskell Language Server (HLS)](https://github.com/haskell/haskell-language-server),
which [GHCup](https://www.haskell.org/ghcup/) can install for you.

See the [LSP](lsp.md) module for the available LSP key bindings.
