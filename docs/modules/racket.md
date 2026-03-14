# Prelude Racket

!!! Note

    This module builds on top of the shared [Lisp Base](lisp.md) module.

## Racket Mode

This module bundles
[racket-mode](https://github.com/greghendershott/racket-mode),
a major mode for editing Racket source files. It's
automatically used for `.rkt` files.

Racket Mode provides:

- Syntax highlighting and indentation for Racket code
- REPL integration
- Definition navigation
- Unicode input method for entering special characters (e.g. `lambda` -> `λ`)

## Key Bindings

| Key | Command | Description |
|-----|---------|-------------|
| <kbd>M-RET</kbd> | `racket-run` | Run the current file in the REPL |
| <kbd>M-.</kbd> | `racket-repl-visit-definition` | Jump to definition |

The common Lisp coding hook is also enabled, providing `smartparens-strict-mode`
and `rainbow-delimiters`.
