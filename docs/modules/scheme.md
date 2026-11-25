# Prelude Scheme

!!! Note

    This module builds on top of the shared [Lisp Base](lisp.md) module.

## Package Prerequisites

You'll need a Scheme, and the Geiser package for it. Geiser supports
many Scheme implementations, each through a package of its own
(`geiser-guile`, `geiser-chicken`, `geiser-mit`, etc). Prelude installs
only Geiser itself, so you'll have to install the package for your
Scheme (e.g. <kbd>M-x package-install RET geiser-guile</kbd>). The
supported implementations are:

- Guile 2.2 or better
- Chicken 4.8.0 or better
- MIT/GNU Scheme 9.1 or better
- Chibi Scheme 0.7 or better
- Chez Scheme 9.4 or better
- Gambit 4.9 or better
- Racket 6.0 or better

## Scheme Mode

The module uses the `scheme-mode` that comes with Emacs, with the
settings that the [Lisp Base](lisp.md) module gives every Lisp.

## Geiser

This module bundles [Geiser](https://www.nongnu.org/geiser/), a popular
interactive programming environment for Scheme. People familiar with
Common Lisp's SLIME will feel right at home with Geiser.

Geiser starts a REPL by itself when you open a Scheme file, as it needs
one for completion and documentation, and keeps the history of the REPL
in Prelude's `savefile` directory. You can also start a REPL with
<kbd>M-x geiser</kbd>.

## Key Bindings

These come from Geiser:

<!-- markdownlint-disable MD013 -->

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>C-c C-z</kbd> | `geiser-mode-switch-to-repl` | Switch to the REPL |
| <kbd>C-x C-e</kbd> | `geiser-eval-last-sexp` | Evaluate the expression before point |
| <kbd>C-M-x</kbd> | `geiser-eval-definition` | Evaluate the definition at point |
| <kbd>C-c C-b</kbd> | `geiser-eval-buffer` | Evaluate the buffer |
| <kbd>C-c C-k</kbd> | `geiser-compile-current-buffer` | Compile and load the buffer |
| <kbd>C-c C-d d</kbd> | `geiser-doc-symbol-at-point` | Show the documentation of the symbol at point |

<!-- markdownlint-enable MD013 -->

Run <kbd>C-h m</kbd> in a Scheme buffer for all key bindings.
