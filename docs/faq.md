# Frequently Asked Questions

## What's the origin of the name Emacs Prelude?

I wanted an Emacs experience that was both as sleek and as powerful
as the legendary sports car [Honda Prelude](https://en.wikipedia.org/wiki/Honda_Prelude).
I also wanted a name with `el` in it. :-)

## Why doesn't Prelude use `use-package`?

While `use-package` provides a nice way of structuring your
configuration (especially if you're into single-file setups), it also adds a layer of complexity as it's just a macro
that expands to some "traditional" configuration code. One aspect of `use-package` that's a bit tricky is where to place cross-package configuration, as it can potentially go to different configuration blocks.

Given how modular the structure of Prelude is, there's relatively little to be gained by adopting `use-package` everywhere, but end users are free to use `use-package` for their personal configuration.

!!! Note

    I have a stripped-down version of Prelude for personal use, based on `use-package` [here](https://github.com/bbatsov/emacs.d).
    I guess it might be of interest to some of you.

**Update (2023): ** There are now plans to include `use-package` in Emacs 29, which will likely increase its prominence. Prelude 1.2 auto-installs `use-package` and newer Prelude modules might make use of `use-package`. `prelude-vertico` is one such example.

## Why does Prelude use MELPA instead of MELPA Stable by default?

Mostly because many package authors/maintainers don't have the habit to cut
"stable" releases of their projects. It seems that's changing for the better
in recent years, so Prelude's defaults might change down the road.

## Why is Zenburn the default color theme?

No particular reason other than the fact that I like it a lot and happen to maintain
its Emacs port. I believe it's pretty nice improvement over the default Emacs theme, but your perspective might be different.
