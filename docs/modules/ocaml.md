# Prelude OCaml

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## Package Prerequisites

You'll need [OPAM](https://opam.ocaml.org/) and the OCaml language
server:

```sh
opam install ocaml-lsp-server
```

## OCaml Mode

The module uses [neocaml](https://github.com/bbatsov/neocaml), a
tree-sitter powered major mode for OCaml, which also comes with support
for Dune files and utop. It installs the grammars it needs by itself.

`subword-mode` is enabled, so the editing commands understand
`CamelCase`, and the language server takes care of completion, type
information, diagnostics, navigation and refactoring. With Eglot (the
default LSP client) the module also turns on
[ocaml-eglot](https://github.com/tarides/ocaml-eglot), which adds the
OCaml-specific features of the language server.

## Key Bindings

These come from ocaml-eglot, and work only with Eglot:

<!-- markdownlint-disable MD013 -->

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>C-c C-a</kbd> | `ocaml-eglot-alternate-file` | Switch between the `.ml` and the `.mli` file |
| <kbd>C-c C-t</kbd> | `ocaml-eglot-type-enclosing` | Show the type of the expression at point |
| <kbd>C-c \|</kbd> | `ocaml-eglot-destruct` | Destruct the expression at point (e.g. into a `match`) |
| <kbd>C-c C-l</kbd> | `ocaml-eglot-find-definition` | Jump to the definition (like <kbd>M-.</kbd>) |

<!-- markdownlint-enable MD013 -->

See the [LSP](lsp.md) module for Prelude's own Eglot commands, and the
documentation of neocaml for its key bindings. Run <kbd>C-h m</kbd> in
an OCaml buffer for all key bindings.
