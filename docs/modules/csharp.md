# Prelude C\#

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## Package Prerequisites

You'll need the .NET SDK and a language server. Eglot knows about
[OmniSharp](https://github.com/OmniSharp/omnisharp-roslyn) (`omnisharp`)
and [csharp-ls](https://github.com/razzmatazz/csharp-language-server)
(`csharp-ls`). The latter is the easier one to install:

```sh
dotnet tool install --global csharp-ls
```

lsp-mode can install a server for you (`M-x lsp-install-server`).

## C# Mode

Emacs comes with two major modes for C#, and the module uses the
tree-sitter powered `csharp-ts-mode`. Prelude offers to install the
`c-sharp` grammar the first time you open a C# file (see
`prelude-treesit-auto-install`). Without the grammar you get the classic
`csharp-mode`.

The version of the grammar depends on your Emacs: `csharp-ts-mode` in
Emacs 29 was written for a much older grammar than the one in Emacs 30,
and each of them loses most of its highlighting with the other's
grammar. If you upgrade from Emacs 29 to 30, delete the old grammar
(`libtree-sitter-c-sharp` in the `tree-sitter` folder of your Emacs
directory) and let Prelude install it again.

`subword-mode` is enabled, so the editing commands understand
`CamelCase`.

## Key Bindings

See the [LSP](lsp.md) module for the available LSP key bindings.
