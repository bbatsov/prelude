# Prelude Swift

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## Package Prerequisites

For the proper functioning of this module, you'll need the following on
your system:

- a Swift toolchain (`swift`, `swiftc`)
- `sourcekit-lsp` (bundled with the Xcode command line tools or a Swift toolchain)

## Swift Mode

The module uses the tree-sitter powered
[swift-ts-mode](https://github.com/rechsteiner/swift-ts-mode) for
editing Swift code. Prelude offers to install the `swift` grammar the
first time you open a Swift file (see `prelude-treesit-auto-install`).
There's no classic mode to fall back to, so without the grammar Swift
files open in `fundamental-mode`.

## LSP support

The module starts an LSP session automatically via Prelude's
`prelude-lsp-enable`, using whichever client `prelude-lsp-client`
selects (Eglot by default). Eglot has no built-in entry for Swift, so
the module registers `sourcekit-lsp` for it. With lsp-mode the module
installs and loads [lsp-sourcekit](https://github.com/emacs-lsp/lsp-sourcekit),
lsp-mode's client for `sourcekit-lsp`.

## Key Bindings

See the [LSP](lsp.md) module for the available LSP key bindings. Run
<kbd>C-h m</kbd> in a Swift buffer for all key bindings.
