# Prelude Julia

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## Package Prerequisites

All you need is Julia itself (`julia` should be on your `PATH`).

## Julia Mode

The module uses [julia-mode](https://github.com/JuliaEditorSupport/julia-emacs)
for editing Julia code. Type a LaTeX-style name like `\alpha` followed
by <kbd>TAB</kbd> to get the Unicode symbol.

## REPL

[julia-repl](https://github.com/tpapp/julia-repl) runs a Julia REPL in a
terminal buffer and lets you send code to it:

<!-- markdownlint-disable MD013 -->

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>C-c C-z</kbd> | `julia-repl` | Start (or switch to) the REPL |
| <kbd>C-c C-c</kbd> | `julia-repl-send-region-or-line` | Send the region or the current line |
| <kbd>C-c C-b</kbd> | `julia-repl-send-buffer` | Send the buffer |
| <kbd>C-c C-a</kbd> | `julia-repl-activate-parent` | Activate the project of the current file |
| <kbd>C-c C-d</kbd> | `julia-repl-doc` | Show the documentation of the symbol at point |

<!-- markdownlint-enable MD013 -->

Run <kbd>C-h m</kbd> for the rest of them.

## LSP support

The language server
([LanguageServer.jl](https://github.com/julia-vscode/LanguageServer.jl))
is a Julia package, so starting it takes a bit more than running an
executable. That's handled by
[eglot-jl](https://github.com/non-Jedi/eglot-jl) for Eglot and
[lsp-julia](https://github.com/gdkrmr/lsp-julia) for lsp-mode. Both of
them install the server the first time it's needed, which takes a
while. If Eglot gives up waiting, bump `eglot-connect-timeout`.

See the [LSP](lsp.md) module for the available LSP key bindings.
