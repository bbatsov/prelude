# Prelude Ruby

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## Package Prerequisites

You'll need Ruby, and a language server for the IDE features. Eglot
looks for [Solargraph](https://solargraph.org/) (`solargraph`) and
[Ruby LSP](https://shopify.github.io/ruby-lsp/) (`ruby-lsp`), in that
order:

```sh
gem install ruby-lsp
```

## Ruby Mode

The module uses the tree-sitter powered `ruby-ts-mode` that comes with
Emacs. Prelude offers to install the `ruby` grammar the first time you
open a Ruby file (see `prelude-treesit-auto-install`). Without the
grammar you get the classic `ruby-mode`.

`subword-mode` is enabled, so the editing commands understand
`CamelCase`, and Emacs doesn't add encoding comments
(`ruby-insert-encoding-magic-comment`), which Ruby hasn't needed since
version 2.0.

## inf-ruby

The module bundles [inf-ruby](https://github.com/nonsequitur/inf-ruby),
which runs a Ruby REPL (e.g. `irb` or `pry`) in an Emacs buffer and lets
you send code to it from your Ruby buffers.

## yari

The module bundles [yari](https://github.com/hron/yari.el), which looks
up Ruby's RI documentation.

## Key Bindings

<!-- markdownlint-disable MD013 -->

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>C-c C-s</kbd> | `inf-ruby` | Start a Ruby REPL |
| <kbd>C-c C-z</kbd> | `ruby-switch-to-inf` | Switch to the REPL |
| <kbd>C-c C-r</kbd> | `ruby-send-region` | Send the region to the REPL |
| <kbd>C-M-x</kbd> | `ruby-send-definition` | Send the definition at point to the REPL |
| <kbd>C-c C-l</kbd> | `ruby-load-file` | Load a file in the REPL |
| <kbd>C-h R</kbd> | `yari` | Look up the documentation of a class or method |

<!-- markdownlint-enable MD013 -->

See the [LSP](lsp.md) module for the available LSP key bindings. Run
<kbd>C-h m</kbd> in a Ruby buffer for all key bindings.
