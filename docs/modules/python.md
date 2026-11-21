# Prelude Python

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## Package Prerequisites

You'll need Python and a language server. Eglot knows how to start
quite a few of them, e.g.
[basedpyright](https://docs.basedpyright.com/),
[Pyright](https://github.com/microsoft/pyright) and
[python-lsp-server](https://github.com/python-lsp/python-lsp-server)
(`pylsp`), and uses the first one it finds. lsp-mode supports the same
servers.

## Python Mode

The module uses the tree-sitter powered `python-ts-mode` that comes
with Emacs. Prelude offers to install the `python` grammar the first
time you open a Python file (see `prelude-treesit-auto-install`).
Without the grammar you get the classic `python-mode`.

A few things are set up on top of that:

- `subword-mode` is enabled, so the editing commands understand
  `CamelCase`
- `imenu` shows a flat list of definitions (`Class.method`) instead of
  a tree
- `PYTHONPATH` is copied from your shell when Emacs wasn't started
  from a terminal (and so didn't inherit the shell's environment)

Code completion, navigation and documentation come from the language
server. Prelude used to bundle `anaconda-mode` for those, but that's no
longer the case.

## Key Bindings

Whenever you are editing Python code run <kbd>C-h m</kbd> to look at
the Python mode key bindings. The ones you'll use the most are
<kbd>C-c C-p</kbd> (start a Python shell), <kbd>C-c C-c</kbd> (send the
buffer to it) and <kbd>C-c C-r</kbd> (send the region).

See the [LSP](lsp.md) module for the available LSP key bindings.

## Syntax checking

Prelude ships with [Flycheck](https://github.com/flycheck/flycheck),
an on the fly syntax checker. It shows the diagnostics of the language
server, and can also run tools like
[Ruff](https://docs.astral.sh/ruff/),
[Flake8](https://flake8.pycqa.org/) and [Pylint](https://www.pylint.org/)
when they are installed. Use <kbd>C-c ! s</kbd> to pick a checker
manually.

## Formatting

The [Apheleia](apheleia.md) module formats Python code on save (with
Black, unless you configure another formatter).
