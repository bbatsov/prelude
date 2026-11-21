# Prelude Emacs Lisp

!!! Note

    This module builds on top of the shared [Lisp Base](lisp.md) module.

## Elisp-mode

The module establishes some sensible defaults for `elisp-mode` and
shortens its modeline name to "EL".

It establishes a few extra keybidings (inspired by SLIME):

- `C-c C-z` (`prelude-visit-ielm`)
- `C-c C-c` (`eval-defun`)
- `C-c C-b` (`eval-buffer`)

The module also recompiles Elisp files on save, when they belong to
Prelude itself (or your personal config) and have been byte-compiled
before. That keeps their `.elc` files from getting stale.

## IELM

IELM is an Elisp REPL bundled with Emacs. Prelude tweaks a bit it's default
configuration to align it with the `elisp-mode` configuration.

## elisp-slime-nav

The module bundles [elisp-slime-nav](https://github.com/purcell/elisp-slime-nav),
which allows you to jump to definitions with `M-.` (use
`M-,` to jump back) and describe a symbol with
`C-c C-d (C-)d`.

## Minibuffer evaluation

`smartparens-mode` is conditionally enabled for `eval-expression` (`M-:`) command.
