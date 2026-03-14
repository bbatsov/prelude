# Prelude JavaScript

!!! Note

    This module builds on top of the shared [Programming](programming.md) module.

## js2-mode

This module replaces the built-in `js-mode` with
[js2-mode](https://github.com/mooz/js2-mode), a more powerful
JavaScript editing mode that features:

- A full JavaScript parser with accurate syntax highlighting
- Smart indentation
- Imenu support for easy navigation (`js2-imenu-extras-mode`)

`js2-mode` is automatically used for `.js`, `.cjs`, `.mjs`, and `.pac` files,
as well as files using the `node` interpreter.

## json-mode

The module also installs [json-mode](https://github.com/joshwnj/json-mode) for
editing JSON files.

## Configuration

- **CamelCase** aware editing is enabled (via `subword-mode`)
- `electric-layout-mode` is configured to insert newlines after semicolons

Run <kbd>C-h m</kbd> to see all available key bindings.
