# Prelude Swift

!!! Note

    This module builds on top of the shared [Programming](programming.md) module.

## Package Prerequisites

For the proper functioning of this module, you'll need the following on
your system:

- a Swift toolchain (`swift`, `swiftc`)
- `sourcekit-lsp` (bundled with the Xcode command line tools or a Swift toolchain)

## Swift Mode

The module uses the tree-sitter powered `swift-ts-mode` for editing
Swift code, so you'll need the `swift` tree-sitter grammar installed.
Prelude ships a recipe for it, so `M-x treesit-install-language-grammar
RET swift` will fetch and build it for you.

Whenever you are editing Swift code run <kbd>C-h m</kbd> to look at the
Swift mode key bindings.

## LSP support

The module starts an LSP session automatically via Prelude's
`prelude-lsp-enable`, using whichever client `prelude-lsp-client`
selects (Eglot by default). Eglot has no built-in entry for Swift, so
the module registers `sourcekit-lsp` for it. lsp-mode users get Swift
support through the `lsp-sourcekit` package.
