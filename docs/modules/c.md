# Prelude C/C++

!!! Note

    This module builds on top of the shared [Programming](programming.md) module.

## cc-mode

Emacs comes with powerful C/C++ programming support through the built-in
`cc-mode`. This module provides sensible defaults for `cc-mode` and all modes
derived from it (e.g. `java-mode`, `php-mode`, etc.).

Prelude configures the following defaults:

- **Indentation style**: K&R (`c-default-style` set to `"k&r"`)
- **Indent offset**: 4 spaces (`c-basic-offset` set to `4`)
- **Substatement opening braces** are not indented
  (`substatement-open` offset set to `0`)

You can override any of these in your personal config:

```emacs-lisp
(setq prelude-c-mode-common-hook
      (lambda ()
        (setq c-default-style "linux"
              c-basic-offset 8)))
```

## Makefile Mode

The module also provides configuration for `makefile-mode`:

- Tabs are allowed in Makefiles (since they are syntactically required)
- `indent-tabs-mode` is enabled

## Key Bindings

Run <kbd>C-h m</kbd> in any C/C++ buffer to see all available key bindings.
