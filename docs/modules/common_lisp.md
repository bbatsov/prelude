# Prelude Common Lisp

!!! Note

    This module builds on top of the shared [Lisp Base](lisp.md) module.

## lisp-mode

`lisp-mode` gets the defaults of the "Lisp Base" module. `.cl` files
and SBCL's `.sbclrc` are opened in it as well.

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
of SLIME with features like stickers (non-disruptive value tracing),
multiple REPLs, flex completion out of the box, and a more polished
inspector, you can install it in your personal config instead. Note that
SLIME and Sly conflict with each other - use one or the other, not both.

To switch, drop something like this into a file under
`~/.emacs.d/personal/` (and remove `prelude-common-lisp` from your
`prelude-modules.el`, or skip loading SLIME some other way):

```emacs-lisp
(use-package sly
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl"))
```

Then start it with `M-x sly`.
