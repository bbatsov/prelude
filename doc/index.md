# Emacs Prelude

Prelude is an Emacs distribution that aims to enhance the default
Emacs experience.  Prelude alters a lot of the default settings,
bundles a plethora of additional packages and adds its own core
library to the mix. The final product offers an easy to use Emacs
configuration for Emacs newcomers and lots of additional power for
Emacs power users.

!!! Important

    Prelude is compatible **ONLY with GNU Emacs 25.1+**. In general you're
    advised to always run Prelude with the latest stable Emacs release.

You can support the development of Prelude via
[GitHub Sponsors](https://github.com/sponsors/bbatsov),
[ko-fi](https://www.ko-fi.com/bbatsov),
[PayPal](https://www.paypal.me/bbatsov) and
[Patreon](https://www.patreon.com/bbatsov).

## Features

* Improved UX, that's still in line with Emacs traditions
* Sane defaults of baseline Emacs functionality
* Automatic installation of many major programming modes on demand
* A curated set of 3rd party packages to enhance the base functionality
* Simple modular architecture
* Easy customization

## Philosophy

Prelude's philosophy is quite simple:

* simple
* easy to understand and extend
* stable
* a foundation for you to build upon, as opposed to some end-user product

This means that it intentionally doesn't pack all the bells and whistles that it could.
Prelude aims to enhance the classic Emacs experience without deviating a lot from it - e.g.
it would never enable something like `evil-mode` (vim keybindings) by default and so on.

All the third-party packages that it bundles are carefully vetted and are known to be of
good quality and to have reliable maintainers. That generally means that Prelude's unlikely
to immediate adopt some shiny new package, that has established tried and true alternatives.

In practice this translates to the following:

* Prelude is less opinionated than distros like Spacemacs and Doom Emacs (meaning it's closer to the standard Emacs experience)
* Prelude installs relatively few additional packages by default
* Most modules in Prelude are opt-in instead of opt-out (you'll notice the default config enables only a handful of modules)
* Most modules (e.g. modules for programming languages) are pretty short and feature setup only for essential packages (in some cases that be just the major mode for the language in question)
* You don't really need to track Prelude's upstream - you're encouraged to just fork it and use it as the basis for your own configuration.

Remember that the ultimate goal of every Emacs user is to create an Emacs setup that reflects their own experience, needs, goals and ideas. Just like Lisp,
Emacs is nothing but a raw building material for the perfect editing experience.
