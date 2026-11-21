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

There's a tree-sitter powered
[kotlin-ts-mode](https://gitlab.com/bricka/emacs-kotlin-ts-mode) as
well. The module doesn't use it, because it needs Emacs 30 and its
highlighting works only with the unreleased version of the Kotlin
grammar, so there's no version of the grammar that Prelude could pin.
It's easy to opt in if that doesn't bother you:

```emacs-lisp
(use-package kotlin-ts-mode
  :ensure t
  :init
  (add-to-list 'major-mode-remap-alist '(kotlin-mode . kotlin-ts-mode))
  :hook (kotlin-ts-mode . (lambda ()
                            (run-hooks 'prelude-kotlin-mode-hook))))
```

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
