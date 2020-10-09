# Prelude Common Lisp

!!! Note

    This module builds on top of the shared [Lisp Base](lisp.md) module.

## lisp-mode

Not much to say here, as `lisp-mode` is configured in the "Lisp Base" module.

## SLIME

This module bundles [SLIME](https://docs.cider.mx), a popular interactive
programming environment for SLIME, and enables many of its essential features.

The default config assumes the usage of [Clozure CL](https://github.com/Clozure/ccl) on macOS and
of [SBCL](http://www.sbcl.org/) everywhere else. That's something you can easily
tweak via `slime-default-lisp`.

You can fire SLIME with `M-x slime`.
