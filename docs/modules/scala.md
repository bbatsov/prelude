# Prelude Scala

!!! Note

    This module builds on top of the shared
    [Programming](programming.md) module and the
    [LSP](lsp.md) module.

## Package Prerequisites

You'll need a JDK, and [Metals](https://scalameta.org/metals/) (the
Scala language server) for the IDE features. The easiest way to install
it is with [Coursier](https://get-coursier.io/):

```sh
cs install metals
```

Make sure the `metals` binary is in your `$PATH`.

## Scala Mode

The module uses [scala-mode](https://github.com/hvesalai/emacs-scala-mode)
for editing Scala code. `subword-mode` is enabled, so the editing
commands understand `CamelCase`, and the language server is started
when it's installed.

## Key Bindings

See the [LSP](lsp.md) module for the available LSP key bindings. Run
<kbd>C-h m</kbd> in a Scala buffer for all key bindings.
