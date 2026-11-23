# Upgrading

Most of the time upgrading Prelude is a matter of running
<kbd>M-x prelude-update</kbd> and restarting Emacs (see
[Updating Prelude](installation.md#updating-prelude)). This page covers
the releases where there's more to it. The
[changelog](https://github.com/bbatsov/prelude/blob/master/CHANGELOG.md)
has the full list of changes in every release.

## Upgrading to Prelude 2.0

Prelude 2.0 is a major release that modernizes the entire
distribution around Emacs 29+ features. Here's what you need to know:

### Emacs 29.1 is now required

Prelude no longer supports Emacs 28 or older. If you haven't
upgraded yet, now's the time. Emacs 29 brings built-in tree-sitter,
Eglot (LSP client), `use-package`, and many other improvements that
Prelude 2.0 takes full advantage of.

### Tree-sitter support

For languages with both classic and tree-sitter modes (e.g.,
`python-mode` / `python-ts-mode`), Prelude uses the tree-sitter mode
when its grammar is available. When it isn't, Prelude offers to
install the grammar the first time you open such a file, and uses the
classic mode if you say no. Set `prelude-treesit-auto-install` to
`always` to skip the question, or to `nil` to never install grammars.
`M-x prelude-treesit-install-grammars` installs the grammars for all
of your enabled modules in one go. Some modules use tree-sitter modes
that install their own grammars (e.g., `prelude-ocaml` uses
`neocaml`).

See the [Emacs manual][ts-grammar] for more on tree-sitter grammars.

[ts-grammar]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Language-Grammar.html

### Built-in LSP via Eglot

Prelude 2.0 adds LSP support to most language modules using Eglot
(built-in since Emacs 29) as the default client. If you were
previously using `prelude-lsp` with lsp-mode, note that:

- The module has been renamed to `prelude-lsp-mode`.
- Set `(setq prelude-lsp-client 'lsp-mode)` in `personal/preload`
  to keep using lsp-mode.
- The default is now Eglot - no extra packages needed.
- Eglot keybindings live under `C-c C-l` (rename, code
  actions, format, etc.).

### Removed packages and modules

Several outdated packages have been replaced by built-in alternatives:

| Removed             | Replacement                          |
|---------------------|--------------------------------------|
| `nlinum`            | Built-in `display-line-numbers-mode` |
| `anzu`              | Built-in `isearch-lazy-count`        |
| `epl`               | Built-in `package-upgrade-all`       |
| `smex` (binding)    | `execute-extended-command`           |
| `anaconda-mode`     | LSP (Eglot/lsp-mode)                 |
| `js2-mode`          | `js-ts-mode` + LSP                   |
| `tide`              | `typescript-ts-mode` + LSP           |
| `alchemist`         | LSP                                  |
| `go-projectile`     | Removed (unmaintained)               |
| `prelude-selectrum` | Use `prelude-vertico` instead        |

### Keybinding changes

- `C-x p` no longer opens `proced` (reserved for `project.el`).
- `C-x C-m` runs `execute-extended-command` instead of `smex`.
- `C-c c` is bound to `org-capture`.
- `C-c C-l` prefix is used for Eglot commands in programming
  modes.

### What you should do

1. **Update Emacs** to 29.1 or newer.
2. **Pull the latest Prelude** and restart Emacs. Packages will
   be updated automatically.
3. **Review your `prelude-modules.el`** - if you had
   `prelude-lsp` enabled, rename it to `prelude-lsp-mode`. If
   you want Eglot (recommended), you don't need any LSP module
   enabled.
4. **Install tree-sitter grammars** for your languages of choice
   (optional but recommended).
5. **Check your personal config** for references to removed
   packages (`nlinum`, `anzu`, `epl`, `anaconda-mode`,
   `js2-mode`, `tide`, `alchemist`).
