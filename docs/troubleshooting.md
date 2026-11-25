# Troubleshooting

## Finding out what's wrong

<kbd>M-x prelude-doctor</kbd> checks what Prelude needs from your
system and lists what's missing: programs like `git` and a C compiler,
the tree-sitter grammars of the modules you've enabled (and their
language servers, with Eglot as the LSP client), and modules that do
the same job. It also tells you
about the modules that were added to Prelude since you made your copy
of `personal/prelude-modules.el`. Press <kbd>g</kbd> in its buffer to
check again after you've installed something.

When Emacs complains during startup, run it like this to get a
backtrace that says where the error comes from:

```shell
emacs --debug-init
```

If the trouble started after you changed your own config, move the
files out of `personal/` (or comment out half of a file) and bring them
back one at a time. To check whether a problem has anything to do with
your config at all, try a pristine copy of Prelude without touching
your setup:

```shell
git clone https://github.com/bbatsov/prelude.git /tmp/prelude
emacs --init-directory=/tmp/prelude
```

## Errors after updating Prelude

Update with <kbd>M-x prelude-update</kbd> rather than a plain
`git pull`: it also updates the packages, which the new code may
depend on, and recompiles Prelude. If you did pull by hand, run
<kbd>M-x prelude-update-packages</kbd> and
<kbd>M-x prelude-recompile-init</kbd>, and restart Emacs.

A warning about "outdated byte-code" on startup means that Prelude
found compiled init files left behind by an older version and removed
them. Restart Emacs once and it's gone.

## "No language server for ..."

Prelude starts a language server only if it's installed, and tells you
once when it isn't. With Eglot, <kbd>M-x prelude-doctor</kbd> lists the
servers of the languages you've enabled that have a tree-sitter mode.
Install the server named on the page of the
language's [module](modules/index.md), make sure Emacs can find it on
your `PATH`, and reopen the file (or run <kbd>M-x eglot</kbd>).

## Tree-sitter grammars that don't install

Installing a grammar means compiling it, so you need `git` and a C
compiler (`gcc` or `clang`). Without the grammar Prelude falls back to
the classic major mode, and you can retry any time with
<kbd>M-x prelude-treesit-install-grammars</kbd>.

## "Modules ... do the same job"

Some modules are alternatives to each other (Vertico, Helm, Ivy and
Ido; Company and Corfu). Enabling two of them at once makes them fight
over the same keys, so Prelude warns you. Keep one per group in
`personal/prelude-modules.el`.

## Keys with Super don't work

Many desktop environments keep the Super (Windows or Command) key for
themselves, and terminals can't send it at all. Every command on a
Super key has another binding (see [Usage](usage.md)), and you can
turn the Super bindings off with `prelude-super-keybindings`.

## No spell checking

Prelude checks your spelling with Flyspell, which needs the `aspell`
program and a dictionary for your language. When `aspell` isn't
installed, spell checking is simply off. On macOS you can get it with
Homebrew:

```shell
brew install aspell
```

On Linux distros - just use your distro's package manager.

## Ugly colors in the terminal Emacs version

If your Emacs looks considerably uglier in a terminal (compared to the
GUI version) try adding this to your `.bashrc` or `.zshrc`:

```bash
export TERM=xterm-256color
```

Source the `.bashrc` file and start Emacs again.

## Packages that fail to install

When a package can't be installed Prelude refreshes the package list
and tries once more, and warns you if that fails too. The usual reason
is the network (or MELPA having a bad moment). Run
<kbd>M-x package-refresh-contents</kbd> and restart Emacs to try again.

## Warnings on arrow navigation in editor buffers

This is not a bug - it's a feature! I firmly believe that the one true
way to use Emacs is by using it the way it was intended to be used (as
far as navigation is concerned at least).

If you'd like to be take this a step further and disable the arrow key navigation
completely put this in your personal config:

```emacs-lisp
(setq guru-warn-only nil)
```

To disable `guru-mode` completely add the following snippet to your
personal Emacs config:

```emacs-lisp
(setq prelude-guru nil)
```

## Customized C-a behavior

Prelude overrides `C-a` to behave as described in this
[blog post about smarter navigation][smart-bol]. If you
don't like that simply add this to your personal config:

```emacs-lisp
(define-key prelude-mode-map (kbd "C-a") nil)
```

In terminal emulators (`ansi-term`, `vterm`, `eat`) <kbd>C-a</kbd> is
passed on to the shell, as you'd expect.

[smart-bol]: https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/

## Poor ido matching performance on large datasets

Prelude's `ido` module swaps the default `ido` flex matching with the
more powerful [ido-flx](https://github.com/lewang/flx).

The sorting algorithm `flx` uses is more complex, but yields better results.

On slower machines, it may be necessary to lower `flx-ido-threshold` to
ensure a smooth experience.

```emacs-lisp
(setq flx-ido-threshold 1000)
```

You can always disable the improved sorting algorithm all together like this:

```emacs-lisp
(flx-ido-mode -1)
```

## Slow startup in GUI Emacs on macOS or Linux

GUI Emacs doesn't inherit the environment of your shell, so Prelude uses
[exec-path-from-shell](https://github.com/purcell/exec-path-from-shell) to
fetch `PATH` and friends from it at startup (Emacs started from a terminal
inherits them already, so this is skipped there). By default that starts an
interactive login shell, which can take a good part of a second with a heavy
shell config. If you set `PATH` in your login files (e.g. `~/.zprofile` or
`~/.bash_profile`), a login shell is enough, and much faster. Add this to your
`personal/preload` config:

```emacs-lisp
(setq exec-path-from-shell-arguments '("-l"))
```

## Windows compatibility

While everything in Prelude should work fine in Windows, I test it only
with GNU/Linux & macOS, so there might be Windows-specific problems from time to
time. This situation will probably improve over time.
