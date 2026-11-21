# Prelude Lua

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## Lua Mode

This module bundles
[lua-mode](https://github.com/immerrr/lua-mode) for editing Lua
source files.

Prelude configures the following defaults:

- **Indent level**: 2 spaces
- **String content indentation** is enabled
- **Nested block alignment** is disabled for a cleaner look

## Key Bindings

| Key | Command | Description |
|-----|---------|-------------|
| <kbd>C-c C-b</kbd> | `lua-send-buffer` | Send buffer to Lua process |
| <kbd>C-c C-f</kbd> | `lua-send-defun` | Send current function |
| <kbd>C-c C-r</kbd> | `lua-send-region` | Send selected region |
| <kbd>C-c C-z</kbd> | `lua-show-process-buffer` | Show Lua process buffer |

## LSP support

The module starts a language server when there's one installed. Eglot
looks for
[lua-language-server](https://github.com/LuaLS/lua-language-server).
See the [LSP](lsp.md) module for the available LSP key bindings.
