# Prelude OCaml

!!! Note

    This module builds on top of the shared
    [Programming](programming.md) module.

## Overview

Prelude provides a modern OCaml development experience
powered by TreeSitter and LSP:

- [neocaml](https://github.com/bbatsov/neocaml) - a
  TreeSitter-powered major mode for OCaml (includes built-in
  support for Dune files and utop)
- [ocaml-eglot](https://github.com/tarides/ocaml-eglot) -
  OCaml-specific LSP extensions via Eglot (enabled when
  `prelude-lsp-client` is set to `eglot`, the default)

LSP integration provides code completion, type information,
diagnostics, code navigation, and refactoring out of the box.

## Prerequisites

You need [OPAM](https://opam.ocaml.org/) and the OCaml LSP
server installed:

```sh
opam install ocaml-lsp-server
```

!!! Note

    This module requires Emacs 29.1+ (for TreeSitter support).
