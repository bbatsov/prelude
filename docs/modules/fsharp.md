# Prelude F\#

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## Package Prerequisites

All you need is the .NET SDK. The language server
([FsAutoComplete](https://github.com/ionide/FsAutoComplete)) is
installed for you the first time you open an F# file.

## F# mode

This module uses
[`fsharp-mode`](https://github.com/fsharp/emacs-fsharp-mode) with
almost no extra configuration:

- `subword-mode` is enabled, so the editing commands understand
  `CamelCase`
- the REPL (<kbd>C-c C-s</kbd>) is started with `dotnet fsi`

## LSP support

With Eglot (the default) the module installs
[eglot-fsharp](https://github.com/fsharp/emacs-fsharp-mode), which
downloads FsAutoComplete and tells Eglot how to start it. lsp-mode
comes with its own F# support, which can install the server as well.

See the [LSP](lsp.md) module for the available LSP key bindings.
