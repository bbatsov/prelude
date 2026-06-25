# Prelude TypeScript

!!! Note

    This module builds on top of the shared [Programming](programming.md) module.

## Packages

This module relies on Emacs' built-in tree-sitter support and LSP, so it
doesn't pull in any third-party packages:

- `typescript-ts-mode` / `tsx-ts-mode` - tree-sitter powered major modes
  for TypeScript and TSX (built into Emacs 29+)

## Features

Code intelligence is provided by an LSP server (Eglot by default):

- Code completion (via company-mode)
- Syntax checking (via Flymake or Flycheck)
- Eldoc integration for inline type information
- Navigation, refactoring, and documentation lookup

## Prerequisites

The tree-sitter modes need the TypeScript grammars installed. You can grab
them with:

```emacs-lisp
(treesit-install-language-grammar 'typescript)
(treesit-install-language-grammar 'tsx)
```

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
