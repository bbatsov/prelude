# Upgrading

Most of the time upgrading Prelude is a matter of running
<kbd>M-x prelude-update</kbd> and restarting Emacs (see
[Updating Prelude](installation.md#updating-prelude)). This page covers
the releases where there's more to it. The
[changelog](https://github.com/bbatsov/prelude/blob/master/CHANGELOG.md)
has the full list of changes in every release.

## Upgrading to Prelude 2.2

### How modules set up major modes

Prelude used to set up a major mode with a `prelude-*-mode-defaults`
function on a hook of its own, like `prelude-ruby-mode-hook`, which it
ran from the hooks of the mode. Now there's one setup function per mode,
right on the hooks of the mode: `prelude-ruby-setup` is on
`ruby-mode-hook` and `ruby-ts-mode-hook`, for instance, and
`prelude-prog-setup` is on `prog-mode-hook`. It runs before the
functions you add to the same hooks, so those have the last word (they
used to run before Prelude's setup).

What that means for your personal config:

- Functions that you add to a `prelude-*-mode-hook` (or to
  `prelude-lisp-coding-hook`) still run, after Prelude's setup, but the
  hooks are obsolete and they'll go away in a future release. Add the
  functions to the hooks of the modes instead. `M-x prelude-doctor`
  lists the obsolete hooks that you're using.
- If you ran one of those hooks from the hook of another mode, to give
  that mode Prelude's setup, that no longer does anything, as Prelude's
  setup isn't on the hooks anymore. Add the setup function to the hook
  instead, e.g. `(add-hook 'fennel-mode-hook #'prelude-lisp-coding-setup)`.
- A function of yours that has to run before Prelude's setup of a mode,
  because it changes something the setup looks at, now runs after it.
  Add it to the hook at a depth below -50, e.g.
  `(add-hook 'python-mode-hook #'my-python-setup -60)`.
- The `prelude-*-mode-defaults` functions are gone.

To keep Prelude's setup out of a mode, which you used to do by removing
its defaults function from its hook, remove the setup function from the
hooks of the mode:

```emacs-lisp
(remove-hook 'ruby-mode-hook #'prelude-ruby-setup)
(remove-hook 'ruby-ts-mode-hook #'prelude-ruby-setup)
```

### The theme

[Tokyo Night](https://github.com/bbatsov/tokyo-night-emacs) replaced
Zenburn as the default theme, with its light variant during the day and
the dark one at night (see [Color Themes](configuration.md#color-themes)).
To keep Zenburn, put this in a file in `personal/preload`:

```emacs-lisp
(setq prelude-theme 'zenburn)
```

### Key bindings

- <kbd>M-y</kbd> is Emacs's own `yank-pop` again (which lets you pick
  from the kill ring), unless your completion module binds its own,
  and <kbd>s-y</kbd> is gone. <kbd>C-h C-m</kbd> is back to
  `view-order-manuals`.
- The Go module's test commands moved to <kbd>C-c C-t t</kbd>,
  <kbd>C-c C-t f</kbd> and <kbd>C-c C-t p</kbd>, and `go-run` to
  <kbd>C-c C-r</kbd>.
- The Ivy module no longer binds <kbd>C-c C-r</kbd>, <kbd>C-c g</kbd>,
  <kbd>C-c j</kbd>, <kbd>C-c a</kbd> and <kbd>C-x l</kbd>, the Org
  module no longer binds <kbd>C-c b</kbd>, and the Helm module no
  longer binds <kbd>C-c w</kbd>.
- The Vertico module leaves <kbd>M-g e</kbd> and <kbd>M-g f</kbd> to
  avy.
- <kbd>C-c I</kbd> opens your personal config (`prelude-user-init-file`)
  instead of Prelude's `init.el`.

### Packages

- Prelude installs its packages with `use-package` and `:ensure t`.
  `prelude-require-package` and `prelude-require-packages` still work,
  but they're obsolete, so use `use-package` in your personal config as
  well.
- Prelude no longer installs or sets up `browse-kill-ring`,
  `discover-my-major`, `smartrep`, `gist`, `company-auctex`, `elpy` and
  the `flycheck-eglot` package (Flycheck has `flycheck-eglot-mode` of
  its own now), and on Emacs 30 and newer `which-key` and
  `editorconfig`, which are built in there. Packages that you installed
  before stay installed, but set them up in your personal config if
  you want to keep using them.
- The ERC module connects to Libera.Chat, and its commands and options
  have Prelude names (`prelude-erc-start`, `prelude-erc-servers`, etc.).
  The old ones still work, but they're obsolete.

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
