# Prelude LSP

## Overview

This module provides a base configuration for the
[Language Server Protocol (LSP)](https://microsoft.github.io/language-server-protocol/)
via [lsp-mode](https://emacs-lsp.github.io/lsp-mode/) and
[lsp-ui](https://emacs-lsp.github.io/lsp-ui/).

LSP allows Emacs to communicate with language servers to provide features like
autocompletion, documentation lookup, code navigation, refactoring, and
diagnostics for many programming languages.

!!! Note

    This is a foundation module used by other language modules
    (e.g. [Go](go.md), [Dart](dart.md), [Scala](scala.md)).
    You typically don't
    need to enable it directly - it will be pulled in automatically by the
    language modules that need it.

## Packages

- [lsp-mode](https://emacs-lsp.github.io/lsp-mode/) - the LSP client
- [lsp-ui](https://emacs-lsp.github.io/lsp-ui/) - UI
  enhancements for lsp-mode (sideline info, peek views, docs)

## Key Bindings

All LSP key bindings use the <kbd>C-c C-l</kbd> prefix:

| Key | Command | Description |
|-----|---------|-------------|
| <kbd>C-c C-l .</kbd> | `lsp-ui-peek-find-definitions` | Peek at definition |
| <kbd>C-c C-l ?</kbd> | `lsp-ui-peek-find-references` | Peek at references |
| <kbd>C-c C-l r</kbd> | `lsp-rename` | Rename symbol |
| <kbd>C-c C-l x</kbd> | `lsp-workspace-restart` | Restart LSP workspace |
| <kbd>C-c C-l w</kbd> | `lsp-ui-peek-find-workspace-symbol` | Find workspace symbol |
| <kbd>C-c C-l i</kbd> | `lsp-ui-peek-find-implementation` | Find implementation |
| <kbd>C-c C-l d</kbd> | `lsp-describe-thing-at-point` | Describe thing at point |
| <kbd>C-c C-l e</kbd> | `lsp-execute-code-action` | Execute code action |

Additionally, `xref-find-definitions` (<kbd>M-.</kbd>) and
`xref-find-references` (<kbd>M-?</kbd>) are remapped to use LSP peek views.

## UI Features

The following `lsp-ui` features are enabled by default:

- **Sideline**: shows diagnostics and code actions in the sideline
- **Doc**: shows documentation on hover
- **Peek**: peek-style code navigation (definitions,
  references, implementations)

## Installing Language Servers

You'll need to install the appropriate language server for each
language you want to use. See the
[lsp-mode documentation](https://emacs-lsp.github.io/lsp-mode/page/languages/)
for a complete list of supported languages and their servers.
