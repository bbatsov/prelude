# Prelude Rust

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## Package Prerequisites

For the proper functioning of this module, you'll need to install the
following packages in your system:

- `rustc` (Rust compiler)
- `cargo` (Rust package manager)
- `rustfmt` (Rust tool for formatting code)
- `rust-analyzer` (Rust Language Server)

[rustup](https://rustup.rs/) can install all of them.

## Rust Mode

The module uses the tree-sitter powered `rust-ts-mode` that comes with
Emacs. Prelude offers to install the `rust` grammar the first time you
open a Rust file (see `prelude-treesit-auto-install`). There's no
classic mode to fall back to, unless you have installed `rust-mode`
yourself.

`subword-mode` is enabled, so the editing commands understand
`CamelCase`.

The module also installs `ron-mode` for
[RON](https://github.com/ron-rs/ron) files.

## Formatting

The buffer is formatted when you save it. That's done by
`rust-analyzer` (which runs `rustfmt`), so it happens only when the
language server is running. To turn it off:

```emacs-lisp
(setq prelude-format-on-save nil)
```

Prelude leaves the formatting to [Apheleia](apheleia.md) when that
module is enabled.

Rust buffers are not saved automatically by `super-save`, as saving
formats the code and you don't want that to happen while you're in the
middle of typing something.

## Syntax checking

Prelude ships with [Flycheck](https://github.com/flycheck/flycheck),
an on the fly syntax checker. Flycheck has support for Rust. Rust is
automatically setup in flycheck, by executing
`flycheck-rust-setup`. If the current file is part of a Cargo project,
flycheck is configured according to the Cargo project layout.

## Cargo integration

`cargo-minor-mode` is enabled in Rust buffers. It lets you run Cargo
commands without leaving the buffer, like <kbd>C-c C-c C-b</kbd> for
`cargo-process-build`, <kbd>C-c C-c C-t</kbd> for `cargo-process-test`
and <kbd>C-c C-c C-r</kbd> for `cargo-process-run`.

## Key Bindings

See the [LSP](lsp.md) module for the available LSP key bindings.
