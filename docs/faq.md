# Frequently Asked Questions

## What's the origin of the name Emacs Prelude?

I wanted an Emacs experience that was both as sleek and as
powerful as the legendary sports car
[Honda Prelude](https://en.wikipedia.org/wiki/Honda_Prelude).
I also wanted a name with `el` in it. :-)

## Does Prelude use `use-package`?

It does. For a long time it didn't: `use-package` wasn't a part of
Emacs, and Prelude's modules were small enough to get by without it.
That changed when `use-package` was included in Emacs 29. Prelude's
core and modules install and configure their packages with it now, and
you're encouraged to do the same in your personal config:

```emacs-lisp
(use-package some-package
  :ensure t)
```

## Why does Prelude use MELPA instead of MELPA Stable by default?

Mostly because many package authors/maintainers don't have
the habit to cut "stable" releases of their projects. It seems
that's changing for the better in recent years, so Prelude's
defaults might change down the road.

## Why is Tokyo Night the default color theme?

No particular reason other than the fact that I like it a lot
and happen to maintain its Emacs port. It comes with a light
variant as well, so Prelude can use that one during the day (or when
your system is in light mode) and the dark one at night. I believe
it's pretty nice improvement over the default Emacs theme, but your
perspective might be different.

Prelude's default used to be Zenburn (another theme I maintain). See
[Color Themes](configuration.md#color-themes) if you want it back.
