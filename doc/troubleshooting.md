# Troubleshooting

## Updating bundled packages

Generally it's a good idea to do a package update before running
updating Prelude, since the latest Prelude code might depend on newer
versions of the bundled packages than you would currently have
installed.

If you're doing manual Prelude updates you should always do a package update first.

`M-x package-list-packages RET U x`

That's not necessary if you're using `M-x prelude-update`, since it
will automatically update the installed packages.

## Problems with flyspell-mode

Prelude makes heavy use of the flyspell-mode package for spell
checking of various things. The proper operation of flyspell depends
on the presence of the `aspell` program and an `en` dictionary on your
system. You can install `aspell` and the dictionary on macOS with
`homebrew` like this:

```bash
brew install aspell --with-lang=en
```

On Linux distros - just use your distro's package manager.

## Ugly colors in the terminal Emacs version

If your Emacs looks considerably uglier in a terminal (compared to the
GUI version) try adding this to your `.bashrc` or `.zshrc`:

```bash
export TERM=xterm-256color
```

Source the `.bashrc` file and start Emacs again.

## MELPA error on initial startup

If you get some http connection error related to the MELPA repo
just do a manual `M-x package-refresh-contents` and restart Emacs
afterwards.

## Warnings on arrow navigation in editor buffers

This is not a bug - it's a feature! I firmly believe that the one true
way to use Emacs is by using it the way it was intended to be used (as
far as navigation is concerned at least).

If you'd like to be take this a step further and disable the arrow key navigation
completely put this in your personal config:

```lisp
(setq guru-warn-only nil)
```

To disable `guru-mode` completely add the following snippet to your
personal Emacs config:

```lisp
(setq prelude-guru nil)
```

## Customized C-a behavior

Prelude overrides `C-a` to behave as described
[here](http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/). If
you don't like that simply add this to your personal config:

```lisp
(global-set-key [remap move-beginning-of-line]
                'move-beginning-of-line)
```

## Poor ido matching performance on large datasets

Prelude's `ido` module swaps the default `ido` flex matching with the
more powerful [ido-flx](https://github.com/lewang/flx).

The sorting algorithm `flx` uses is more complex, but yields better results.

On slower machines, it may be necessary to lower `flx-ido-threshold` to
ensure a smooth experience.

```lisp
(setq flx-ido-threshold 1000)
```

You can always disable the improved sorting algorithm all together like this:

```lisp
(flx-ido-mode -1)
```

## Windows compatibility

While everything in Prelude should work fine in Windows, I test it only
with GNU/Linux & macOS, so there might be Windows-specific problems from time to
time. This situation will probably improve over time.
