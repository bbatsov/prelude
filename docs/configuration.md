# Configuration

## User Interface

Historically Prelude had adopted a very minimalistic approach to UI and
had hidden by default Emacs's menu bar and tool bar. This was changed a bit
in Prelude 1.1 for the sake of being more approachable to newcomers to Emacs
and now the menu bar is displayed by default. The tool bar is still hidden, as
it's quite big and not that useful.

!!! Tip

    You can toggle the menu bar by pressing `F12`.

Furthermore, Prelude 1.1 displays line numbers (via `global-nlinum-mode`), just
like most "modern" editors and IDEs do these days. You can go back to the way
things were by setting `prelude-minimalistic-ui` to `t` in `personal/preload` or
by adding the following snippets to your personal config:

``` emacs-lisp
(global-nlinum-mode -1)
(menu-bar-mode -1)
```

!!! Note

    The first approach is better as it would prevent those UI elements from
    appearing temporarily.

## Color Themes

Emacs provides a dozen of
built-in themes you can use out-of-the-box by invoking the `M-x
load-theme` command.

[Zenburn](https://github.com/bbatsov/zenburn-emacs) is the default
color theme in Prelude, but you can change it at your discretion. Why
Zenburn? I (and lots of hackers around the world) find it pretty neat
for some reason. Personally I find the default theme pretty tiresome
for the eyes, that's why I took that "controversial" decision to
replace it. You can, of course, easily go back to the default (or
select another theme entirely).

To disable Zenburn just put in your personal config the following
line:

```emacs-lisp
(disable-theme 'zenburn)
```

Or you can use another theme altogether by adding something in `personal/preload` like:

```emacs-lisp
(setq prelude-theme 'tango)
```

!!! Note

    To use a non-built-in theme, like [Solarized](https://github.com/bbatsov/solarized-emacs),
    you'll have to install it from MELPA first by `M-x package-install RET solarized-theme`. Then add

```emacs-lisp
(setq prelude-theme 'solarized-dark)
```
in `personal/preload`.

Finally, if you don't want any theme at all, you can add this to your
`personal/preload`:

```emacs-lisp
(setq prelude-theme nil)
```

## Personalizing

All files you create under the `personal/` directory are yours for
personalization.  There is no single special personal config file --
any files you create in the `personal/` directory will be loaded in
lexicographical order.  The overall loading precedence is:

1.  `personal/preload/*`
2.  `core/`
3.  `personal/prelude-modules.el` (or deprecated `prelude-modules.el`)
4.  `personal/*`

### Personalization Example

Suppose you want to configure `go-mode` to autoformat on each save.  You
can create a file in `personal/`, let's call this one
`config-go-mode.el` and add the following to it.

```emacs-lisp
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 2)))
```

### General Tips

**Fork** (instead of cloning) the official Prelude repo and add your
own touch to it. You're advised to **avoid changing stuff outside of
the personal folder** to avoid having to deal with git merge conflicts
in the future.

If you'd like to add some auto installation of packages in your
personal config use the following code:

```emacs-lisp
(prelude-require-packages '(some-package some-other-package))
```

If you require just a single package you can also use:

```emacs-lisp
(prelude-require-package 'some-package)
```

### Preloading personal config

Sometimes you might want to load code before Prelude has started loading. Prelude will automatically preload all
Emacs Lisp files in your `personal/preload` directory. Note that at this point you can't using anything from
Prelude, except a few variables like `prelude-dir`, etc (since nothing is yet loaded).

### Disabling whitespace-mode

Although `whitespace-mode` is awesome, some people might find it too
intrusive. You can disable it in your
personal config with the following bit of code:

```emacs-lisp
(setq prelude-whitespace nil)
```

If you like `whitespace-mode`, but prefer it to not automatically
cleanup your file on save, you can disable that behavior by setting
`prelude-clean-whitespace-on-save` to `nil` in your config file with:

```emacs-lisp
(setq prelude-clean-whitespace-on-save nil)
```

### Disable flyspell-mode

If you're not fond of spellchecking on the fly:

```emacs-lisp
(setq prelude-flyspell nil)
```

### Disable automatic formatting on save

If you prefer not to automatically format your file on save, you can disable that behavior by setting
`prelude-format-on-save` to `nil` in your config file with:

```emacs-lisp
(setq prelude-format-on-save nil)
```

Currently this only affects automated formatting of Typescript files.

### Disable Super-based keybindings

Out-of-the-box Prelude will create two versions of many keybindings in `prelude-mode`:

* One "traditional" version with a prefix like `Control`
* One "alternative" version with a prefix like `Super`

The reason for this is that there are generally more options for short keybindings with `Super` - e.g. you can
have `s-p`, `s-g`, etc. There's, however, a problem lying here as well - some operating systems and
desktop environments might be making heavy use of such keybindings. (in most cases those would intercept them before Emacs does).
`exwm` also uses those heavily. You prevent Prelude from creating such keybindings via `prelude-super-keybindings`:

```emacs-lisp
(setq prelude-super-keybindings nil)
```

### Configuration per file or directory

Some of these settings (those that don't need to be pre-loaded) can also be set
on a per-file or directory basis by using a file local variable or a
`.dir-locals.el` file.
