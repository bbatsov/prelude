# Prelude Zig

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## Package Prerequisites

You'll need `zig` and its language server,
[zls](https://github.com/zigtools/zls). Make sure the version of `zls`
matches the version of Zig you're using.

## Zig Mode

The module uses [zig-mode](https://github.com/ziglang/zig-mode) for
editing Zig code. It formats the buffer with `zig fmt` every time you
save it. To turn that off:

```emacs-lisp
(setq zig-format-on-save nil)
```

`subword-mode` is enabled, so the editing commands understand
`camelCase`.

## Key Bindings

<!-- markdownlint-disable MD013 -->

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>C-c C-b</kbd> | `zig-compile` | Build the project |
| <kbd>C-c C-r</kbd> | `zig-run` | Run the current file |
| <kbd>C-c C-t</kbd> | `zig-test-buffer` | Run the tests in the current file |
| <kbd>C-c C-f</kbd> | `zig-format-buffer` | Format the buffer |

<!-- markdownlint-enable MD013 -->

See the [LSP](lsp.md) module for the available LSP key bindings.
