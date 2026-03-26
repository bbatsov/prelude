# Prelude Clojure

!!! Note

    This module builds on top of the shared [Lisp Base](lisp.md) module.

## Clojure Mode

This module bundles `clojure-mode`, a major mode for programming in Clojure,
and some sensible defaults for it.

Prelude uses `clojure-mode` rather than the newer `clojure-ts-mode` because:

- `clojure-ts-mode` requires Emacs 30+, while Prelude supports Emacs 29.1+.
- CIDER and `clj-refactor` still depend on `clojure-mode` for some APIs.
- `clojure-ts-mode` is still under active development and not yet at full
  feature parity with `clojure-mode`.

### Using clojure-ts-mode (Emacs 30+)

If you're on Emacs 30+ and want to try tree-sitter based Clojure
support, install `clojure-ts-mode` in your personal config:

```emacs-lisp
(use-package clojure-ts-mode
  :ensure t)
```

`clojure-ts-mode` auto-remaps `clojure-mode` buffers when installed,
so no additional configuration is needed. It will also auto-install
the required tree-sitter grammar on first use. See the
[clojure-ts-mode documentation](https://github.com/clojure-emacs/clojure-ts-mode)
for details.

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

I'm the author of CIDER and `inf-clojure` and the primary
maintainer of `clojure-mode`. I'm also a co-maintainer of
`clj-refactor`. I guess I love Clojure! :-)
