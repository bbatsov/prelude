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

## Indentation

C, C++ and Objective-C code is indented in the K&R style, with 4
spaces, in the tree-sitter modes as well as in the classic ones:

```c
int main(void)
{
    if (x) {
        foo();
    }
    return 0;
}
```

For the tree-sitter modes that's done with `c-ts-mode-indent-style`
and `c-ts-mode-indent-offset` (called `c-ts-indent-offset` in Emacs
31). The classic modes get a style called `prelude`, which is the
`k&r` style of `cc-mode` with `c-basic-offset` set to 4. The other
modes built on `cc-mode`, like `java-mode`, keep their usual styles.

Prelude changes these only if they still have Emacs's default values,
so to use something else just set them, in your personal config or with
<kbd>M-x customize</kbd>:

```emacs-lisp
;; the tree-sitter modes (c-ts-indent-offset in Emacs 31)
(setq c-ts-mode-indent-style 'linux
      c-ts-mode-indent-offset 8)
;; the classic modes
(setq c-default-style '((c-mode . "linux")
                        (c++-mode . "linux")
                        (java-mode . "java")
                        (awk-mode . "awk")
                        (other . "gnu")))
```

The rest of the C setup (`subword-mode` and the language server) is
in `prelude-c-mode-common-defaults`, which is run from
`prelude-c-mode-common-hook` in C, C++ and Objective-C buffers. Add your own
functions to that hook to change it:

```emacs-lisp
(add-hook 'prelude-c-mode-common-hook
          (lambda () (subword-mode -1)))
```

## Key Bindings

See the [LSP](lsp.md) module for the available LSP key bindings. Run
<kbd>C-h m</kbd> in any C/C++ buffer to see all available key bindings.
