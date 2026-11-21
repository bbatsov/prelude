# Prelude YAML

!!! Note

    This module builds on top of the [LSP](lsp.md) module.

## YAML Mode

The module uses the tree-sitter powered `yaml-ts-mode` that comes with
Emacs. Prelude offers to install the `yaml` grammar the first time you
open a YAML file (see `prelude-treesit-auto-install`). Without the
grammar you get [yaml-mode](https://github.com/yoshiki/yaml-mode),
which is installed the first time it's needed.

## Features

The YAML modes don't derive from `prog-mode`, so the module enables a
few useful features itself:

- **whitespace-mode** for visualizing whitespace (important in
  a whitespace-sensitive format like YAML)
- **subword-mode** for CamelCase aware editing
- **Automatic whitespace cleanup** on save

## LSP support

A language server is started when there's one installed, and nothing
is said about it when there isn't. Eglot looks for
[yaml-language-server](https://github.com/redhat-developer/yaml-language-server):

```sh
npm install -g yaml-language-server
```

See the [LSP](lsp.md) module for the available LSP key bindings.
