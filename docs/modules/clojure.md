# Prelude Clojure

!!! Note

    This module builds on top of the shared [Lisp Base](lisp.md) module.

## Clojure Mode

This module bundles `clojure-mode`, a major mode for programming in Clojure,
and some sensible defaults for it.

## CIDER

This module also bundles [CIDER](https://docs.cider.mx), a popular interactive
programming environment for Clojure.

Intentionally, Prelude doesn't install by default popular CIDER plugins like
`clj-refactor`, `sayid`, etc, as those can be overwhelming to newcomers and
are easy to setup if you need them.

## CIDER Alternatives

Depending on your preferences you might want to use `inf-clojure` or `clojure-lsp`
alongside/instead of CIDER, but you'll have to set them up yourselves.

## Fun trivia

I'm the author of CIDER and `inf-clojure` and the primary maintainer of `clojure-mode`. I'm also a co-maintainer of `clj-refactor`. I guess I love Clojure! :-)
