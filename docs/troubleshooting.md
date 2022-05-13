# Troubleshooting

## Updating bundled packages

Generally it's a good idea to do a package update before running
updating Prelude, since the latest Prelude code might depend on newer
versions of the bundled packages than you would currently have
installed.

If you're doing manual Prelude updates you should always do a package update first.

    M-x package-list-packages RET U x

That's not necessary if you're using `M-x prelude-update`, since it
will automatically update the installed packages.

## Problems with flyspell-mode

Prelude makes heavy use of the flyspell-mode package for spell
checking of various things. The proper operation of flyspell depends
on the presence of the `aspell` program and an `en` dictionary on your
system. You can install `aspell` and the dictionary on macOS with
`homebrew` like this:

```shellsession
$ brew install aspell --with-lang=en
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

```emacs-lisp
(setq guru-warn-only nil)
```

To disable `guru-mode` completely add the following snippet to your
personal Emacs config:

```emacs-lisp
(setq prelude-guru nil)
```

## Customized C-a behavior

Prelude overrides `C-a` to behave as described
[here](http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/). If
you don't like that simply add this to your personal config:

```emacs-lisp
(global-set-key [remap move-beginning-of-line]
                'move-beginning-of-line)
```

If you're using term-mode or ansi-term-mode, the above will not
restore the default behaviour of sending the C-a key sequence directly
to the terminal. As a workaround, you can remove the C-a binding from
prelude-mode specifically for these as described
[here](https://emacsredux.com/blog/2013/09/25/removing-key-bindings-from-minor-mode-keymaps/)
by adding something like the following to your personal config:

```emacs-lisp
(defun my-term-mode-hook ()
  (let ((oldmap (cdr (assoc 'prelude-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "C-a") nil)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(prelude-mode . ,newmap) minor-mode-overriding-map-alist)))

(add-hook 'term-mode-hook 'my-term-mode-hook)
```

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

## Windows compatibility

While everything in Prelude should work fine in Windows, I test it only
with GNU/Linux & macOS, so there might be Windows-specific problems from time to
time. This situation will probably improve over time.
