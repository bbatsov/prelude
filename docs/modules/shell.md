# Prelude Shell

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## Shell Script Mode

The module uses the tree-sitter powered `bash-ts-mode` that comes with
Emacs. Prelude offers to install the `bash` grammar the first time you
open a shell script (see `prelude-treesit-auto-install`). Without the
grammar you get the classic `sh-mode`, which is also what
`bash-ts-mode` switches to for the shells it doesn't support.

## Prezto Support

The module recognizes [Prezto](https://github.com/sorin-ionescu/prezto)
configuration files (such as `zlogin`, `zlogout`, `zpreztorc`, `zprofile`,
`zshenv`, and `zshrc`) as shell scripts and automatically sets the shell type to
`zsh` for them.

## LSP support

A language server is started for `sh` and `bash` scripts when there's
one installed, and nothing is said about it when there isn't. Eglot
looks for
[bash-language-server](https://github.com/bash-lsp/bash-language-server):

```sh
npm install -g bash-language-server
```

See the [LSP](lsp.md) module for the available LSP key bindings.
