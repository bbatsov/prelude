# Prelude Apheleia

## Overview

[Apheleia](https://github.com/radian-software/apheleia) is a
unified format-on-save framework for Emacs. On save, it runs the
appropriate external formatter (Prettier, Black, Ruff, gofmt,
rustfmt, ...) asynchronously, then patches the buffer with the
result. The patching is point-preserving and flicker-free - it
feels like the formatter ran instantly with no visible buffer
churn.

Apheleia ships with sensible defaults for dozens of formatters and
many major modes out of the box. Look at `apheleia-formatters` and
`apheleia-mode-alist` for the full list.

## What this module does

Not much:

1. Installs the `apheleia` package.
2. Enables `apheleia-global-mode`.
3. Formats Go code with `goimports` instead of `gofmt` when
   `goimports` is installed.

That's it - no further setup required for the languages Apheleia
already knows about.

## Relationship to language modules

A couple of Prelude's language modules format on save themselves
([Go](go.md) with `gofmt`, [Rust](rust.md) with `rust-analyzer`). They
step aside in the buffers where Apheleia is enabled, so the code isn't
formatted twice.

## Adding a formatter

For languages or tools Apheleia doesn't know about, register a
formatter and tell Apheleia which mode it applies to:

```emacs-lisp
(with-eval-after-load 'apheleia
  (setf (alist-get 'shfmt apheleia-formatters)
        '("shfmt" "-i" "2" "-ci"))
  (setf (alist-get 'sh-mode apheleia-mode-alist) 'shfmt))
```

See the [Apheleia README](https://github.com/radian-software/apheleia)
for the formatter spec and built-in entries you can copy from.

## Disabling per buffer

If you need to skip formatting for a specific buffer, run
<kbd>M-x apheleia-mode</kbd> to toggle it off locally. To skip a whole
project, set a directory-local `apheleia-inhibit` to `t`.
