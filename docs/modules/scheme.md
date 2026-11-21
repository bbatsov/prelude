# Prelude Scheme

!!! Note

    This module builds on top of the shared [Lisp Base](lisp.md) module.

## lisp-mode

Not much to say here, as `scheme-mode` is configured to use Prelude's
default Lisp settings.

## Geiser

This module bundles [Geiser](https://www.nongnu.org/geiser/), a popular interactive
programming environment for Scheme. People familiar with Common Lisp's SLIME will
feel right at home with Geiser.

Note that Geiser supports many Scheme implementations, each through
a package of its own (`geiser-guile`, `geiser-chicken`, `geiser-mit`,
etc). Prelude installs only Geiser itself, so you'll have to install
the package for your Scheme:

- Guile 2.2 or better
- Chicken 4.8.0 or better
- MIT/GNU Scheme 9.1 or better
- Chibi Scheme 0.7 or better
- Chez Scheme 9.4 or better
- Gambit 4.9 or better
- Racket 6.0 or better

You can fire Geiser with `M-x geiser`.
