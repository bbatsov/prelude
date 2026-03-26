# Prelude Common Lisp

!!! Note

    This module builds on top of the shared [Lisp Base](lisp.md) module.

## lisp-mode

Not much to say here, as `lisp-mode` is configured in the "Lisp Base" module.

## SLIME

This module bundles [SLIME](https://common-lisp.net/project/slime/), a popular interactive
programming environment for Common Lisp, and enables many of its essential features.

SLIME supports many Common Lisp implementations. Prelude defaults to
[SBCL](http://www.sbcl.org/) on all platforms. You can change this via
`slime-default-lisp`, or use `M-- M-x slime` to pick an implementation
interactively.

You can start SLIME with `M-x slime`.

## Alternatives

If you prefer [Sly](https://github.com/joaotavora/sly), a modernized fork
of SLIME with features like stickers, improved completion, and a more
polished UI, you can install it in your personal config instead. Note that
SLIME and Sly conflict with each other -- use one or the other, not both.
