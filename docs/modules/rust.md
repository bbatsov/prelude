# Prelude Rust

!!! Note

    This module builds on top of the shared [Programming](programming.md) module.

## Package Prerequisites

For the proper functioning of this module, you'll need to install the
following packages in your system:

* `rustc` (Rust compiler)
* `cargo` (Rust package manager)
* `rustfmt` (Rust tool for formatting code)
* `rust-analyzer` (Rust Language Server, required for `prelude-lsp` feature)

## Rust Mode

Emacs comes with Rust programming support through the built-in
`rust-mode`. Whenever you are editing Rust code run <kbd>C-h m</kbd> to
look at the Rust mode key bindings.

## Syntax highlighting

[tree-sitter-mode](https://emacs-tree-sitter.github.io/) is used for nicer
syntax highlighting.

## Syntax checking

Prelude ships with [Flycheck](https://github.com/flycheck/flycheck),
an on the fly syntax checker. Flycheck has support for Rust. Rust is
automatically setup in flycheck, by executing
`flycheck-rust-setup`. If the current file is part of a Cargo project,
flycheck is configured according to the Cargo project layout.

## Cargo integration

Along with `rust-mode`, `cargo-minor-mode` is also configured. You can
give cargo commands from inside the buffer to run cargo commands, like
<kbd>C-c C-c C-b</kbd> for `cargo-process-build`, <kbd>C-c C-c
C-t</kbd> for `cargo-process-test` and <kbd>C-c C-c C-r</kbd> for
`cargo-process-run`
