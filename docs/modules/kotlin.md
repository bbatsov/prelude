# Prelude Kotlin

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## Package Prerequisites

You'll need a JDK, the Kotlin compiler and a language server:
[kotlin-language-server](https://github.com/fwcd/kotlin-language-server)
(`kotlin-language-server`) is the one both Eglot and lsp-mode know how
to start.

JetBrains are working on an
[official language server](https://github.com/Kotlin/kotlin-lsp) as
well. If you'd like to use that one with Eglot, add it to
`eglot-server-programs`.

## Kotlin Mode

The module uses
[kotlin-mode](https://github.com/Emacs-Kotlin-Mode-Maintainers/kotlin-mode)
for editing Kotlin code, Gradle's Kotlin build files (`*.gradle.kts`)
included.

`subword-mode` is enabled, so the editing commands understand
`CamelCase`.

## Key Bindings

<!-- markdownlint-disable MD013 -->

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>C-c C-z</kbd> | `kotlin-repl` | Start a Kotlin REPL |
| <kbd>C-c C-n</kbd> | `kotlin-send-line` | Send the current line to the REPL |
| <kbd>C-c C-r</kbd> | `kotlin-send-region` | Send the region |
| <kbd>C-c C-c</kbd> | `kotlin-send-block` | Send the current block |
| <kbd>C-c C-b</kbd> | `kotlin-send-buffer` | Send the buffer |

<!-- markdownlint-enable MD013 -->

See the [LSP](lsp.md) module for the available LSP key bindings.

Gradle projects are known to
[Projectile](https://github.com/bbatsov/projectile), so
<kbd>C-c p c c</kbd> compiles the project and <kbd>C-c p P</kbd> runs
its tests.
