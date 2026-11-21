# Prelude TypeScript

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## Packages

This module relies on Emacs' built-in tree-sitter support and LSP, so it
doesn't pull in any third-party packages:

- `typescript-ts-mode` / `tsx-ts-mode` - tree-sitter powered major modes
  for TypeScript and TSX (built into Emacs 29+)

## Features

Code intelligence is provided by an LSP server (Eglot by default):

- Code completion
- Syntax checking (via Flycheck)
- Eldoc integration for inline type information
- Navigation, refactoring, and documentation lookup

## Prerequisites

The tree-sitter modes need the `typescript` and `tsx` grammars. Prelude
offers to install them the first time you open a file that needs them
(see `prelude-treesit-auto-install`). Emacs has no classic mode for
TypeScript, so without the grammars you get `typescript-mode` if you
have that package installed, and `fundamental-mode` otherwise.

For code intelligence you need [Node.js](https://nodejs.org/) and a language
server. The default is
[typescript-language-server](https://github.com/typescript-language-server/typescript-language-server):

```sh
npm install -g typescript typescript-language-server
```

## Format on Save

For automatic formatting on save, enable the
[Apheleia](apheleia.md) module. It runs `prettier` (or another configured
formatter) asynchronously whenever you save a TypeScript buffer.
