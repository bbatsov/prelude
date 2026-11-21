# Prelude CSS

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## CSS Mode

The module uses the tree-sitter powered `css-ts-mode` that comes with
Emacs. Prelude offers to install the `css` grammar the first time you
open a CSS file (see `prelude-treesit-auto-install`). Without the
grammar you get the classic `css-mode`.

The module adds a couple of enhancements on top of it:

- **Indent offset**: 2 spaces (`css-indent-offset` set to `2`)
- **Rainbow mode**:
  [rainbow-mode](https://elpa.gnu.org/packages/rainbow-mode.html)
  is enabled, which colorizes color strings (like `#ff0000` or
  `rgb(255, 0, 0)`) with the color they represent

## LSP support

A language server is started when there's one installed, and nothing
is said about it when there isn't. Eglot looks for
`vscode-css-language-server`:

```sh
npm install -g vscode-langservers-extracted
```

See the [LSP](lsp.md) module for the available LSP key bindings.
