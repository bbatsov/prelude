# Prelude Dart

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## Package Prerequisites

All you need is the Dart SDK (or Flutter, which includes it). The
language server is a part of the SDK (`dart language-server`).

## Dart Mode

The module uses [dart-mode](https://github.com/emacsorphanage/dart-mode)
for editing Dart code.

`subword-mode` is enabled, so the editing commands understand
`CamelCase`.

## LSP support

With Eglot (the default) you get the standard LSP features. See the
[LSP](lsp.md) module for the available key bindings.

With lsp-mode the module also installs
[lsp-dart](https://emacs-lsp.github.io/lsp-dart/), which adds a lot on
top of that:

- Flutter integration
- Tree views
- Run pub
- Debug
- Run tests

Some essential commands (lsp-mode only):

- `lsp-dart-pub-get` - Run pub get or flutter pub get on
  project root.
- `lsp-dart-pub-upgrade` - Run pub upgrade or flutter pub
  upgrade on project root.
- `lsp-dart-pub-outdated` - Run pub outdated or flutter pub
  outdated on project root.
- `lsp-dart-run-all-tests` - Run all tests from project.
- `lsp-dart-run-test-file` - Run all tests from current
  test buffer.

The module also sets lsp-dart up for debugging with
[dap-mode](https://emacs-lsp.github.io/dap-mode/), which takes a
debugger that lsp-dart downloads the first time.

## Key Bindings

With lsp-mode the module adds two keys for lsp-dart's outlines:

<!-- markdownlint-disable MD013 -->

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>C-c C-l D s o</kbd> | `lsp-dart-show-outline` | Show the outline of the file |
| <kbd>C-c C-l D s f</kbd> | `lsp-dart-show-flutter-outline` | Show the Flutter outline |

<!-- markdownlint-enable MD013 -->

See the [LSP](lsp.md) module for the other LSP key bindings.
