# Prelude TypeScript

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## Package Prerequisites

You'll need [Node.js](https://nodejs.org/) and a language server. The
default is
[typescript-language-server](https://github.com/typescript-language-server/typescript-language-server):

```sh
npm install -g typescript typescript-language-server
```

## TypeScript Mode

The module uses the tree-sitter powered `typescript-ts-mode` and
`tsx-ts-mode` that come with Emacs, so it doesn't install any packages.
Prelude offers to install the `typescript` and `tsx` grammars the first
time you open a file that needs them (see
`prelude-treesit-auto-install`). Emacs has no classic mode for
TypeScript, so without the grammars you get `typescript-mode` if you
have that package installed, and `fundamental-mode` otherwise.

`subword-mode` is enabled, so the editing commands understand
`CamelCase`, and the language server takes care of completion,
diagnostics (shown by Flycheck), documentation and navigation.

## Formatting

For automatic formatting on save, enable the [Apheleia](apheleia.md)
module. It runs `prettier` (or another formatter that you configure)
whenever you save a TypeScript buffer.

## Key Bindings

See the [LSP](lsp.md) module for the available LSP key bindings. Run
<kbd>C-h m</kbd> in a TypeScript buffer for all key bindings.
