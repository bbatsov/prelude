# Prelude Elixir

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## Package Prerequisites

You'll need Elixir and a language server, e.g.
[ElixirLS](https://github.com/elixir-lsp/elixir-ls) or
[Lexical](https://github.com/lexical-lsp/lexical). Eglot looks for
their start scripts (`language_server.sh` and `start_lexical.sh`) on
your `PATH`.

## Elixir Mode

On Emacs 30+ the module uses the tree-sitter powered `elixir-ts-mode`
that comes with Emacs. Prelude offers to install the `elixir` grammar
the first time you open an Elixir file (see
`prelude-treesit-auto-install`). Phoenix templates are covered by
`heex-ts-mode`, which needs the `heex` grammar.

On Emacs 29, or when the grammar isn't installed, you get
[elixir-mode](https://github.com/elixir-editors/emacs-elixir). It's
installed the first time it's needed.

`subword-mode` is enabled, so the editing commands understand
`CamelCase`.

Prelude used to bundle `alchemist` for completion, navigation and
running Mix. It's no longer maintained and the language servers have
replaced most of it, so that's no longer the case.

## Key Bindings

See the [LSP](lsp.md) module for the available LSP key bindings.

Mix projects are known to
[Projectile](https://github.com/bbatsov/projectile), so
<kbd>C-c p c c</kbd> compiles the project and <kbd>C-c p P</kbd> runs
its tests.
