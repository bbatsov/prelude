<!-- markdownlint-disable MD013 MD046 -->

# Configuration

## User Interface

Historically Prelude had adopted a very minimalistic approach to UI and
had hidden by default Emacs's menu bar and tool bar. This was changed a bit
in Prelude 1.1 for the sake of being more approachable to newcomers to Emacs
and now the menu bar is displayed by default. The tool bar is still hidden, as
it's quite big and not that useful.

!!! Tip

    You can toggle the menu bar by pressing `F12`.

Furthermore, Prelude displays line numbers (via `global-display-line-numbers-mode`),
just like most "modern" editors and IDEs do these days. You can go back to the way
things were by setting `prelude-minimalistic-ui` to `t` in `personal/preload` or
by adding the following snippets to your personal config:

``` emacs-lisp
(global-display-line-numbers-mode -1)
(menu-bar-mode -1)
```

!!! Note

    The first approach is better as it would prevent those UI elements from
    appearing temporarily.

## Welcome Screen

On startup Prelude greets you with a welcome screen that points you at the
essentials - where your personal configuration lives, how to enable modules,
and the user manual - and gives you quick access to your recent files, known
projects and bookmarks. It's skipped automatically when you start Emacs to open
specific files (e.g. `emacs foo.rb`).

You can bring it back at any time with `M-x prelude-welcome`.

If you'd rather not see it, set `prelude-welcome-screen` to `nil` in
`personal/preload`:

``` emacs-lisp
(setq prelude-welcome-screen nil)
```

## Color Themes

Emacs provides a dozen of
built-in themes you can use out-of-the-box by invoking the `M-x
load-theme` command.

[Tokyo Night](https://github.com/bbatsov/tokyo-night-emacs) is the
default color theme in Prelude: its light variant (`tokyo-night-day`)
during the day and the dark one (`tokyo-night`) at night. Prelude
switches between the two on its own.

The theme is set with `prelude-theme`, in `personal/preload`. It's
either one theme, or a pair of a day theme and a night theme:

```emacs-lisp
;; one theme around the clock
(setq prelude-theme 'tokyo-night-storm)

;; a light theme during the day, a dark one at night
(setq prelude-theme '(modus-operandi . modus-vivendi))
```

What counts as day is up to `prelude-theme-daytime`. By default
Prelude follows the light and dark mode of your system, if Emacs is told
about it. That's the case in Emacs 31 on Wayland (PGTK builds), Windows
and Android, and in the
[Emacs Plus](https://github.com/d12frosted/homebrew-emacs-plus) build
on macOS. Everywhere else the day lasts from 07:00 to 19:00.

You can set the times yourself:

```emacs-lisp
(setq prelude-theme-daytime '("08:30" . "17:00"))
```

or go by the sunrise and the sunset, as long as Emacs knows where you
are:

```emacs-lisp
(setq prelude-theme-daytime 'sun)
(setq calendar-latitude 42.7
      calendar-longitude 23.3)
```

To follow the system with an Emacs that isn't told about its
appearance (e.g. the regular macOS builds) there's the
[auto-dark](https://github.com/LionyxML/auto-dark-emacs) package, which
asks the system every few seconds. Turn off Prelude's theme in
`personal/preload`:

```emacs-lisp
(setq prelude-theme nil)
```

and set up `auto-dark` in your personal config:

```emacs-lisp
(use-package auto-dark
  :ensure t
  :config
  ;; the dark theme goes first
  (setq auto-dark-themes '((tokyo-night) (tokyo-night-day)))
  (auto-dark-mode))
```

If you pick another theme by hand (<kbd>M-x load-theme</kbd>), Prelude
leaves it alone for the rest of the session.

!!! Note

    To use a theme that isn't built into Emacs, like
    [Solarized](https://github.com/bbatsov/solarized-emacs), you'll have
    to install it first with `M-x package-install RET solarized-theme`.
    [Zenburn](https://github.com/bbatsov/zenburn-emacs), Prelude's
    default theme for many years, is the exception:
    `(setq prelude-theme 'zenburn)` is all it takes.

Finally, if you don't want any theme at all, you can add this to your
`personal/preload`:

```emacs-lisp
(setq prelude-theme nil)
```

## Personalizing

Everything under the `personal/` directory is yours. There's no single
special personal config file - every `.el` file directly in
`personal/` is loaded, in alphabetical order. Subdirectories are not
looked into, with the exception of `personal/preload/`.

Prelude loads things in this order:

1. `personal/early-init.el`, from Prelude's own `early-init.el`
2. `personal/preload/`, all the `.el` files in it and in its
   subdirectories
3. Prelude's core
4. `personal/prelude-modules.el`, the list of modules you've enabled
   (the bundled `sample/prelude-modules.el` is used if you don't have
   one)
5. the rest of the `.el` files in `personal/`, `custom.el` (where Emacs
   saves what you set with `M-x customize`) among them

The order matters for Prelude's own options. Many of them are read
while the core is being loaded, so setting them in a regular personal
file comes too late - they belong in `personal/preload/`. See the
[list of options](#prelude-options) for which is which.

`custom.el` is loaded late on purpose: what you've customized is meant
to override Prelude's settings, not the other way around. It's loaded
in alphabetical order with your other personal files, so a file named
`init.el` or `settings.el` comes after it and has the last word.

### Personalization Example

Suppose you want tabs to be 2 columns wide in Go buffers. You can
create a file in `personal/`, let's call this one `config-go.el`, and
add the following to it.

```emacs-lisp
(add-hook 'prelude-go-mode-hook
          (lambda ()
            (setq tab-width 2)))
```

### General Tips

**Fork** (instead of cloning) the official Prelude repo and add your
own touch to it. You're advised to **avoid changing stuff outside of
the personal folder** to avoid having to deal with git merge conflicts
in the future.

If you'd like to add some auto installation of packages in your
personal config, use `use-package` with `:ensure t`:

```emacs-lisp
(use-package some-package
  :ensure t)
```

Prelude installs these the same way it installs its own packages (it
refreshes a stale package list and retries if an install fails), and
`M-x prelude-update-packages` updates them as well.

The older `prelude-require-package` and `prelude-require-packages`
still work, but they're deprecated.

### Preloading personal config

Sometimes you might want to load code before Prelude has started loading. Prelude will automatically preload all
Emacs Lisp files in your `personal/preload` directory. Note that at this point you can't use anything from
Prelude, except a few variables like `prelude-dir`, etc (since nothing is yet loaded).

This is where most of Prelude's options have to be set. A typical preload file looks like this:

```emacs-lisp
;;; personal/preload/settings.el
(setq prelude-theme 'modus-vivendi)
(setq prelude-lsp-client 'lsp-mode)
(setq prelude-super-keybindings nil)
```

### Prelude options

The second column says where an option has to be set to have an effect. `startup` options are read while
Prelude is being loaded, so in Lisp they have to be set in a file in `personal/preload/`. `anywhere` means
that a regular personal file will do as well.

All of them can also be set with `M-x customize-group RET prelude`. Prelude picks its own options out of
`custom.el` before it loads its core, so the `startup` ones work from there too (after a restart). That
covers what Customize itself saves, in the `custom.el` of your personal folder.

Pick one of the two ways for any given option. A preload file decides how Prelude starts, but once
`custom.el` gets loaded its value replaces the one from the preload file, and the two end up disagreeing.

Option | Set it in | Default | What it controls
-------|-----------|---------|-----------------
`prelude-theme` | startup | Tokyo Night | The color theme to load, or a `(DAY . NIGHT)` pair of themes. `nil` leaves the theme alone.
`prelude-theme-daytime` | startup | `system` | When the day theme is used: `system` to follow the system's light and dark mode, a pair of times, or `sun` for sunrise to sunset.
`prelude-minimalistic-ui` | startup | `nil` | Hide the menu bar and the line numbers.
`prelude-welcome-screen` | startup | `t` | Show the welcome screen on startup.
`prelude-super-keybindings` | startup | `t` | Bind commands to keys that use Super.
`prelude-lsp-client` | startup | `eglot` | The LSP client: `eglot` or `lsp-mode`.
`prelude-projectile` | startup | `t` | Set up Projectile and its key bindings.
`prelude-smartparens` | startup | `t` | Set up smartparens.
`prelude-undo-tree` | startup | `t` | Use undo-tree for undo and redo.
`prelude-hippie-expand` | startup | `t` | Bind <kbd>M-/</kbd> to `hippie-expand`.
`prelude-auto-save` | startup | `t` | Save buffers when you switch to another buffer or window, or away from Emacs (`super-save`).
`prelude-spell-checker` | startup | `flyspell` | The spell checker: `flyspell` or `jinx`.
`prelude-whitespace-cleanup-style` | startup | `whitespace-cleanup` | How whitespace is cleaned up on save: the whole file, or with `ws-butler` only the lines you've touched.
`prelude-flyspell` | anywhere | `t` | Check spelling as you type.
`prelude-whitespace` | anywhere | `t` | Highlight whitespace problems (`whitespace-mode`).
`prelude-clean-whitespace-on-save` | anywhere | `t` | Clean up whitespace when a file is saved.
`prelude-guru` | anywhere | `t` | Nudge you towards the Emacs way of doing things (`guru-mode`).
`prelude-format-on-save` | anywhere | `t` | Let the Go and Rust modules format files on save.
`prelude-treesit-auto-install` | anywhere | `ask` | What to do about a missing tree-sitter grammar: `ask`, `always` install it, or `nil`.
`prelude-indent-sensitive-modes` | anywhere | a list of modes | Modes where yanked code isn't re-indented.
`prelude-yank-indent-modes` | anywhere | a list of modes | Extra (non-programming) modes where yanked text is indented.
`prelude-yank-indent-threshold` | anywhere | `1000` | Yanked text longer than this many characters isn't indented.
`prelude-user-init-file` | anywhere | `personal/` | What <kbd>C-c I</kbd> opens: the file or directory with your personal config.

### Personal early-init

Some settings have to be in place before the package system and the first frame are set up (e.g.
`default-frame-alist` tweaks). Put those in `personal/early-init.el` and Prelude's own `early-init.el`
will load it after its own settings, so you can also override those. The file isn't loaded
again together with the rest of `personal/`.

### Fonts

Prelude doesn't touch your fonts. Emacs picks a monospaced font that's
available on your system, and a color font for emoji.

The most reliable way to choose a font yourself is to set it before the
first frame is created, in `personal/early-init.el`:

```emacs-lisp
;; the size is in points
(push '(font . "JetBrains Mono-14") default-frame-alist)
```

That covers every frame, including the ones you open later with
`emacsclient`. The font has to be installed, otherwise Emacs falls back
to its default.

If you'd like a different font for emoji (or for any other script), add
something like this to your personal config:

```emacs-lisp
(set-fontset-font t 'emoji "Noto Color Emoji")
```

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

This affects the modules that format on save (Go and Rust). It has
no effect on [Apheleia](modules/apheleia.md).

### Switch LSP client

Prelude uses Eglot (built-in) as the default LSP client. If you prefer lsp-mode
with its richer UI (peek views, sideline diagnostics), add to `personal/preload`:

```emacs-lisp
(setq prelude-lsp-client 'lsp-mode)
```

See the [LSP module documentation](modules/lsp.md) for details.

### Disable Super-based keybindings

Out-of-the-box Prelude will create two versions of many keybindings in `prelude-mode`:

- One "traditional" version with a prefix like `Control`
- One "alternative" version with a prefix like `Super`

The reason for this is that there are generally more options for short keybindings with `Super` - e.g. you can
have `s-p`, `s-g`, etc. There's, however, a problem lying here as well - some operating systems and
desktop environments might be making heavy use of such keybindings. (in most cases those would intercept them before Emacs does).
`exwm` also uses those heavily. You prevent Prelude from creating such keybindings via `prelude-super-keybindings`,
in `personal/preload`:

```emacs-lisp
(setq prelude-super-keybindings nil)
```

### Disable Projectile

By default, Prelude configures [Projectile](https://github.com/bbatsov/projectile) for project
interaction. To disable this, add the following to your `personal/preload` config.

``` emacs-lisp
(setq prelude-projectile nil)
```

### Disable hippie-expand

By default, Prelude configures `hippie-expand` as a replacement for Emacs' default `dabbrev`. To disable this
behaviour, add the following to your `personal/preload` config.

``` emacs-lisp
(setq prelude-hippie-expand nil)
```

### Disable smartparens

By default, Prelude enables `smartparens`. To disable this behaviour, add the following to
your `personal/preload` config (it has to be set before Prelude's core is loaded).

``` emacs-lisp
(setq prelude-smartparens nil)
```

### Tree-sitter grammars

Prelude uses tree-sitter based major modes (e.g. `python-ts-mode`) when the
grammar for the language is installed. When it isn't, Prelude asks whether to
install it the first time you open a file in that language (this requires a C
compiler), and falls back to the classic major mode if you decline. You can
change that in `personal/preload`:

``` emacs-lisp
;; install missing grammars without asking
(setq prelude-treesit-auto-install 'always)
;; never install grammars, just use the classic modes
(setq prelude-treesit-auto-install nil)
```

Prelude installs the version of a grammar that the tree-sitter mode of
your Emacs was written for, which isn't always the latest one. If the
highlighting of some language looks broken (e.g. after you've upgraded
Emacs, or because the grammar was installed by an older Prelude), delete
its file from the `tree-sitter` folder of your Emacs directory and open
a file of that language again.

`M-x prelude-treesit-install-grammars` installs the grammars for all of your
enabled modules at once, which is handy before going offline.

### Configuration per file or directory

Some of these settings (those that don't need to be pre-loaded) can also be set
on a per-file or directory basis by using a file local variable or a
`.dir-locals.el` file. For instance, to keep Prelude from cleaning up
the whitespace of the files in a project, put this in a `.dir-locals.el`
at its root:

```emacs-lisp
((nil . ((prelude-clean-whitespace-on-save . nil))))
```

### package-user-dir

By default, prelude sets the directory where downloaded modules are installed to `<prelude-dir>/elpa`. If you want
to disable this behaviour, set the following variable to nil in `personal/preload`.

```emacs-lisp
(setq prelude-override-package-user-dir nil)
```
