# Prelude Haskell

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## Package Prerequisites

You'll need GHC and Cabal (or Stack), and a language server for the IDE
features. Both Eglot and lsp-mode use the
[Haskell Language Server (HLS)](https://github.com/haskell/haskell-language-server).
[GHCup](https://www.haskell.org/ghcup/) can install all of them for you.

## Haskell Mode

The module uses [haskell-mode](https://github.com/haskell/haskell-mode)
and turns on a few useful minor modes:

- `subword-mode` - CamelCase aware editing
- `eldoc-mode` - type signatures in the echo area
- `haskell-indentation-mode` - indentation that follows Haskell's
  layout rules
- `interactive-haskell-mode` - interaction with a GHCi process

## Key Bindings

These come from `interactive-haskell-mode`:

<!-- markdownlint-disable MD013 -->

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>C-c C-l</kbd> | `haskell-process-load-file` | Load the file in GHCi |
| <kbd>C-c C-z</kbd> | `haskell-interactive-switch` | Switch to GHCi |
| <kbd>C-c C-t</kbd> | `haskell-process-do-type` | Show the type of the identifier at point |
| <kbd>C-c TAB</kbd> | `haskell-process-do-info` | Show information about the identifier at point |
| <kbd>C-c C-c</kbd> | `haskell-process-cabal-build` | Build the project with Cabal |

<!-- markdownlint-enable MD013 -->

See the [LSP](lsp.md) module for the available LSP key bindings. Run
<kbd>C-h m</kbd> in a Haskell buffer for all key bindings.
