# Prelude PHP

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## Package Prerequisites

You'll need PHP and a language server. Eglot knows how to start
[Phpactor](https://phpactor.readthedocs.io/) (`phpactor`), and lsp-mode
can install [Intelephense](https://intelephense.com/) for you
(`M-x lsp-install-server`).

## PHP Mode

The module uses [php-mode](https://github.com/emacs-php/php-mode) for
editing PHP code. Templates that mix PHP and HTML are better served by
`web-mode` from the [Web](web.md) module.

`subword-mode` is enabled, so the editing commands understand
`CamelCase`.

Emacs 30+ comes with a tree-sitter powered `php-ts-mode` as well. The
module doesn't use it by default, as it needs quite a few grammars
(`M-x php-ts-mode-install-parsers` installs them), but it gets the same
setup when you switch to it:

```emacs-lisp
(add-to-list 'major-mode-remap-alist '(php-mode . php-ts-mode))
```

## Key Bindings

See the [LSP](lsp.md) module for the available LSP key bindings. Run
<kbd>C-h m</kbd> in a PHP buffer for the key bindings of `php-mode`.
