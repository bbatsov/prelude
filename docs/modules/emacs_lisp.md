# Prelude Emacs Lisp

!!! Note

    This module builds on top of the shared [Lisp Base](lisp.md) module.

    This module is enabled by default.

## Emacs Lisp Mode

On top of what the [Lisp Base](lisp.md) module does for every Lisp, the
module sets up `emacs-lisp-mode` with:

- `eldoc-mode`, which shows the arguments of the function at point in
  the echo area
- `rainbow-mode`, which shows color names and color codes (e.g.
  `"#ff0000"`) in their colors
- a shorter mode line name, "EL"

It also recompiles the Emacs Lisp files of Prelude (and of your personal
config) when you save them, if they've been byte-compiled before. That
keeps their `.elc` files from getting stale.

Flycheck checks the docstrings of your code with `checkdoc`, but not in
test files (files named `*-test.el` or `*-tests.el`), where that's mostly
noise.

`Cask`, `Eask` and `Eldev` files (used by the build tools of Emacs
packages) open in `emacs-lisp-mode` as well.

## IELM

IELM is the Emacs Lisp REPL that comes with Emacs. The module gives it
the same Lisp editing helpers (without the whitespace checks) and
`eldoc-mode`, and <kbd>C-c C-z</kbd> in an Emacs Lisp buffer switches to
it (starting it if needed).

## elisp-slime-nav

The module bundles
[elisp-slime-nav](https://github.com/purcell/elisp-slime-nav), which
lets you jump to the definition of the symbol at point with
<kbd>M-.</kbd> and describe it with <kbd>C-c C-d d</kbd>, in Emacs Lisp
buffers and in IELM alike.

## Minibuffer evaluation

When smartparens is on, it's also used in the minibuffer of
`eval-expression` (<kbd>M-:</kbd>).

## Key Bindings

These are the keys that the module adds (some of them inspired by
SLIME):

<!-- markdownlint-disable MD013 -->

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>C-c C-z</kbd> | `prelude-visit-ielm` | Switch to IELM, starting it if needed |
| <kbd>C-c C-c</kbd> | `eval-defun` | Evaluate the top-level form at point |
| <kbd>C-c C-b</kbd> | `eval-buffer` | Evaluate the buffer |
| <kbd>M-.</kbd> | `elisp-slime-nav-find-elisp-thing-at-point` | Jump to the definition of the symbol at point |
| <kbd>M-,</kbd> | `pop-tag-mark` | Jump back |
| <kbd>C-c C-d d</kbd> | `elisp-slime-nav-describe-elisp-thing-at-point` | Describe the symbol at point |

<!-- markdownlint-enable MD013 -->
