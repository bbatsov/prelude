# Prelude JavaScript

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

    This module is enabled by default.

## Package Prerequisites

You'll need [Node.js](https://nodejs.org/) and a language server. The
one Eglot and lsp-mode start by default is
[typescript-language-server](https://github.com/typescript-language-server/typescript-language-server),
which handles JavaScript as well:

```sh
npm install -g typescript typescript-language-server
```

## JavaScript Mode

The module doesn't install any packages - it relies on what comes with
Emacs. JavaScript files (`.js`, `.jsx`, `.mjs` and `.cjs`) are opened in
the tree-sitter powered
`js-ts-mode`, and Prelude offers to install the `javascript` grammar
the first time you open one (see `prelude-treesit-auto-install`).
Without the grammar you get the classic `js-mode`.

On Emacs 31+ `js-ts-mode` needs a second grammar, `jsdoc`, and doesn't
start without it. Prelude installs the two together.

`subword-mode` is enabled, so the editing commands understand
`CamelCase`.

Prelude used to replace `js-mode` with `js2-mode`. These days the
built-in modes and a language server do a better job, so that's no
longer the case.

## JSON

JSON files are handled by this module as well: they are opened in
`json-ts-mode` when the `json` grammar is available, and in the
classic `js-json-mode` otherwise.

A language server is started for them when there's one installed, and
nothing is said about it when there isn't. Eglot looks for
`vscode-json-language-server`:

```sh
npm install -g vscode-langservers-extracted
```

## Key Bindings

See the [LSP](lsp.md) module for the available LSP key bindings. Run
<kbd>C-h m</kbd> to see the key bindings of the major mode.

## Formatting

The [Apheleia](apheleia.md) module formats JavaScript and JSON on save
(with Prettier).
