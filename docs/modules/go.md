# Prelude Go

This module builds on top of the shared [Programming](programming.md)
module, as well as the `prelude-lsp` module.

The following keybindings are set by default, which are not present in
each mode's default bindings:

* <kbd>C-c a</kbd> (`go-test-current-project`)
* <kbd>C-c m</kbd> (`go-test-current-file`)
* <kbd>C-c .</kbd> (`go-test-current-test`)
* <kbd>C-c b</kbd> (`go-run`)
* <kbd>C-h f</kbd> (`godoc-at-point`)

Run <kbd>C-h m</kbd> for all the key bindings and other documentation.

There are two ways to manage projects in Go: `$GOPATH` and with Go
modules. Modules are the newer, recommended method. Read [Using Go
Modules](https://go.dev/blog/using-go-modules) to learn about this, if
you are unfamiliar with the subject. Many of the tools used by Prelude
Go may provide functions that are broken with modules. There is
usually another function that will work properly; when in doubt, use a
function provided by `lsp-mode` which is documented below.

Generics were added to Go in 1.18. `gopls`, the backend for `lsp-mode`
setup herein, supports generics as long as `gopls` itself was built
with 1.18+. Other minor modes may not support generics yet.

## Go Mode

`prelude-go` builds on several useful Go tools, and establishes sensible
defaults. The major mode is `go-mode`. Documentation is available at [github.com/dominikh/go-mode.el](https://github.com/dominikh/go-mode.el)

## Go Projectile

[Projectile](https://github.com/bbatsov/projectile) integration is
provided by [go-projectile](https://github.com/dougm/go-projectile).

This provides:

* Projectile integration
* Switching GOPATH if desired per project (customizable via
  `customize`)
* Ability to download all commonly used `go` tools via <kbd>M-x
  go-projectile-install-tools</kbd> and update them via <kbd>M-x
  go-projectile-update-tools</kbd>
* Very basic refactoring via `go-projectile-rewrite` (uses `gofmt -r`)
* Support for `go get` and `go get -u` via `go-projectile-get` and
  `go-projectile-update`.

See its documentation for details.

## LSP Mode and LSP UI

[LSP](https://microsoft.github.io/language-server-protocol/) (Language
Server Protocol) is a protocol that allows editors to use an external
"language server" to provide features like autocompletion,
documentation, and code navigation rather than implementing these
features separately in each editor. Emacs supports LSP via
`lsp-mode`. The language server used is
[gopls](https://github.com/golang/tools/tree/master/gopls).

To install `gopls`, change to a directory outside of `$GOPATH` or any
module (e.g., `/tmp`) and execute:

```
go install golang.org/x/tools/gopls@latest
```

Ensure that `gopls` is in your `$PATH`.

Excellent documentation for `lsp-mode` and `lsp-ui` are provided at [emacs-lsp.github.io/lsp-mode/](https://emacs-lsp.github.io/lsp-mode/)

If a feature, such as documentation, refactoring, indenting, etc. is
provided by `lsp`, you should use it instead of calling to another
tool. `gopls` is the officially maintained tool that supercedes
functionality in other tools, like `gocode`, and works properly with
modules and generics.

Company support is automatically added that works with `lsp`.

## GoTest

[gotest](https://github.com/nlamirault/gotest.el) is also provided
while editing Go files in order to run tests more easily. The bindings
provided by `prelude-go` are listed at the top because `gotest` does
not set any.
