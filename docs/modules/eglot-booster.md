# Prelude Eglot Booster

## Overview

[eglot-booster](https://github.com/jdtsmith/eglot-booster) speeds up
Eglot by routing its JSON-RPC traffic through
[emacs-lsp-booster](https://github.com/blahgeek/emacs-lsp-booster),
a small Rust wrapper that pre-parses LSP messages before handing
them to Emacs. On verbose servers like `rust-analyzer`, `gopls`, or
`tsserver`, and on large projects, the difference is dramatic (no
more multi-second freezes during completion or diagnostics).

This module is **opt-in** and **requires installing a separate
binary** (`emacs-lsp-booster`). The Emacs side gracefully no-ops if
the binary is missing, so Eglot keeps working as before.

!!! Note

    This module assumes you're using Eglot (Prelude's default LSP
    client). If you've switched `prelude-lsp-client` to `lsp-mode`,
    use the equivalent [`lsp-mode` performance
    flags](https://emacs-lsp.github.io/lsp-mode/page/performance/)
    instead.

## Setup

### 1. Install the booster binary

Download a prebuilt release for your platform:

<https://github.com/blahgeek/emacs-lsp-booster/releases>

Place the `emacs-lsp-booster` binary somewhere on your `PATH` (e.g.
`/usr/local/bin/` or `~/.local/bin/`).

Or build from source:

```sh
cargo install emacs-lsp-booster
```

### 2. Enable the module

Uncomment the `prelude-eglot-booster` line in your
`~/.emacs.d/personal/prelude-modules.el` and restart Emacs. On first
load the module will fetch the `eglot-booster` Emacs package via
`package-vc-install`.

### 3. Verify

Open a file with an active Eglot connection and run
<kbd>M-x list-processes</kbd>. The command of the language server's
process should start with `emacs-lsp-booster`, which confirms that the
booster is in the loop. (Prelude turns off Eglot's event log, so
`eglot-events-buffer` won't tell you.)

## Customization

- `eglot-booster-no-remote-boost` - set to `t` to skip boosting
  servers running over TRAMP. Defaults to `nil`.
- `eglot-booster-io-only` - set to `t` to keep Emacs 30+'s native
  JSON parser (the booster only handles I/O, not bytecode). Useful
  if you want to compare.

## Limitations

The booster only wraps LSP servers that communicate over standard
input/output. Servers using TCP sockets are not boosted.
