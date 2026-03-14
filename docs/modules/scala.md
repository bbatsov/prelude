# Prelude Scala

!!! Note

    This module builds on top of the shared
    [Programming](programming.md) module and the
    [LSP](lsp.md) module.

## Scala Mode

This module bundles [scala-mode](https://github.com/hvesalai/emacs-scala-mode)
for editing Scala source files, with LSP integration enabled by default.

The module enables:

- **subword-mode** for CamelCase aware editing
- **lsp-mode** for IDE-like features via
  [Metals](https://scalameta.org/metals/) (the Scala language
  server)

## Prerequisites

You need to install [Metals](https://scalameta.org/metals/)
for LSP support.
The easiest way is via [Coursier](https://get-coursier.io/):

```sh
cs install metals
```

Make sure the `metals` binary is in your `$PATH`.

## Key Bindings

See the [LSP](lsp.md) module for the available LSP key bindings. Run
<kbd>C-h m</kbd> in a Scala buffer for all key bindings.
