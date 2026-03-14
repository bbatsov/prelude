# Prelude Erlang

!!! Note

    This module builds on top of the shared [Programming](programming.md) module.

## Erlang Mode

This module bundles [erlang-mode](https://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html)
for editing Erlang source files. The mode is part of the official Erlang/OTP distribution.

## Flymake

When `erlang-start` is available, `flymake-mode` is enabled for on-the-fly
syntax checking.

## Projectile Integration

When Projectile is enabled, the module configures `erlang-compile-function` to
use `projectile-compile-project`, so that <kbd>C-c C-k</kbd> compiles using
your project's build tool.

## Wrangler

[Wrangler](https://refactoringtools.github.io/wrangler/) is an Erlang
refactoring tool. To enable it, set the `wrangler-path` variable to the
location of the Wrangler Elisp directory in your personal config:

```emacs-lisp
(setq wrangler-path "/path/to/wrangler/elisp")
```
