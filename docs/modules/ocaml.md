# Prelude OCaml

!!! Note

    This module builds on top of the shared
    [Programming](programming.md) module.

## Overview

Prelude provides a modern OCaml development experience
powered by TreeSitter and LSP:

- [neocaml](https://github.com/bbatsov/neocaml) - a
  TreeSitter-powered major mode for OCaml
- [ocaml-eglot](https://github.com/tarides/ocaml-eglot) -
  OCaml-specific LSP extensions via Eglot
- [dune](https://github.com/ocaml/dune) - major mode for
  Dune project files
- [utop](https://github.com/ocaml-community/utop) - a
  modern OCaml toplevel (REPL)

LSP integration provides code completion, type information,
diagnostics, code navigation, and refactoring out of the box.

## Prerequisites

You need [OPAM](https://opam.ocaml.org/) and the OCaml LSP
server installed:

```sh
opam install ocaml-lsp-server
```

For REPL support, also install utop:

```sh
opam install utop
```

!!! Note

    This module requires Emacs 29.1+ (for TreeSitter support).
