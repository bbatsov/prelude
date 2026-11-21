# Prelude Erlang

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## Package Prerequisites

You'll need Erlang/OTP and a language server. Recent versions of Eglot
start [ELP](https://whatsapp.github.io/erlang-language-platform/)
(`elp`), older ones
[Erlang LS](https://erlang-ls.github.io/) (`erlang_ls`). lsp-mode
supports both.

## Erlang Mode

This module bundles [erlang-mode](https://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html)
for editing Erlang source files. The mode is part of the official Erlang/OTP distribution.

`subword-mode` is enabled, so the editing commands understand
`CamelCase`.

## Projectile Integration

When Projectile is enabled, the module configures `erlang-compile-function` to
use `projectile-compile-project`, so that <kbd>C-c C-k</kbd> compiles using
your project's build tool.

## Key Bindings

See the [LSP](lsp.md) module for the available LSP key bindings. Run
<kbd>C-h m</kbd> in an Erlang buffer for the key bindings of
`erlang-mode`.
