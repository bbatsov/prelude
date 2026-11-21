# Prelude Nix

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## Package Prerequisites

You'll need a language server: [nil](https://github.com/oxalica/nil)
(`nil`) is a good choice, and the one every version of Eglot knows how
to start. Newer versions know about
[nixd](https://github.com/nix-community/nixd) (`nixd`) as well.

## Nix Mode

The module uses [nix-mode](https://github.com/NixOS/nix-mode) for
editing Nix expressions. It comes with a few extras worth knowing:

- `nix-repl` - a Nix REPL
- `nix-format-buffer` - format the buffer with `nixfmt`
- `nix-build` - run `nix-build` for the current directory

## Key Bindings

See the [LSP](lsp.md) module for the available LSP key bindings.
