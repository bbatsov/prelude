# Prelude C/C++

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## Package Prerequisites

You'll need a compiler and a language server. Eglot looks for
[clangd](https://clangd.llvm.org/) and
[ccls](https://github.com/MaskRay/ccls), in that order.

## C and C++ Modes

The module uses the tree-sitter powered `c-ts-mode` and `c++-ts-mode`
that come with Emacs. Prelude offers to install the `c` and `cpp`
grammars the first time you open a file that needs them (see
`prelude-treesit-auto-install`). Without the grammars you get the
classic `c-mode` and `c++-mode`.

`subword-mode` is enabled, so the editing commands understand
`CamelCase`. The module also installs `cmake-mode` for CMake files.

## cc-mode

The classic modes are built on top of `cc-mode`. This module provides
sensible defaults for `cc-mode` and all modes derived from it (e.g.
`java-mode`, `php-mode`, etc.). The tree-sitter modes get the same
indentation style.

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

## Key Bindings

See the [LSP](lsp.md) module for the available LSP key bindings. Run
<kbd>C-h m</kbd> in any C/C++ buffer to see all available key bindings.
