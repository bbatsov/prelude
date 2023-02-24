[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![CI](https://github.com/bbatsov/prelude/workflows/CI/badge.svg)](https://github.com/bbatsov/prelude/actions/workflows/ci.yml)
[![Patreon](https://img.shields.io/badge/patreon-donate-orange.svg)](https://www.patreon.com/bbatsov)
[![Discord](https://img.shields.io/badge/chat-on%20discord-7289da.svg?sanitize=true)](https://discord.gg/3Cf2Qpyry5)

Emacs Prelude
=============

Prelude is an Emacs distribution that aims to enhance the default
Emacs experience.  Prelude alters a lot of the default settings,
bundles a plethora of additional packages and adds its own core
library to the mix. The final product offers an easy to use Emacs
configuration for Emacs newcomers and lots of additional power for
Emacs power users.

Prelude is compatible **ONLY with GNU Emacs 25.1+**. In general you're
advised to always run Prelude with the latest stable Emacs release.

You can support the development of Prelude via
[GitHub Sponsors](https://github.com/sponsors/bbatsov),
[ko-fi](https://ko-fi.com/bbatsov),
[PayPal](https://www.paypal.me/bbatsov) and
[Patreon](https://www.patreon.com/bbatsov).

## Features

* Improved UX, that's still in line with Emacs traditions
* Sane defaults of baseline Emacs functionality
* Automatic installation of many major programming modes on demand
* A curated set of 3rd party packages to enhance the base functionality
* Simple modular architecture
* Easy customization

Check out our [user manual](https://prelude.emacsredux.com) for more information.

## Fast Forward

Assuming you're using an Unix-like OS (`*BSD`, `GNU/Linux`, `macOS`, `Solaris`,
etc), you already have a recent version of Emacs installed, as well as `git` & `curl` you
can skip the whole manual and just type in your favorite shell the
following command:

```shellsession
$ curl -L https://git.io/epre | sh
```

You can now power up your Emacs, sit back and enjoy Prelude.

There are two environment variables you can use to control the
source repository and the installation directory. To change the
installation directory:

```shellsession
$ export PRELUDE_INSTALL_DIR="$HOME/.emacs.d" && curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh | sh
```

To change the source repository:

```shellsession
$ export PRELUDE_URL="https://github.com/yourname/prelude.git" && curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh | sh
```

Note that the installer will back up any existing `.emacs` file or
`.emacs.d` since it will unpack Prelude's code in `.emacs.d`. If
you're doing a manual install make sure you don't have a `.emacs` file
or back up your existing `.emacs.d` directory manually.

**Important:** Don't forget to adjust your `prelude-modules.el` file in your personal directory
once the installation is done. By default most of the modules
that ship with Prelude are **not** loaded.

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

More installation options are discussed [here](https://prelude.emacsredux.com/en/latest/installation/).

## User Manual

While Emacs Prelude is pretty simple at its core, it does have some specifics that
are worth learning - e.g. configuration options, load order of modules and personal settings
and so on.

Check out our [user manual](https://prelude.emacsredux.com) for more information.

You can also find a lot of information about specific Prelude features and the rationale behind them on
my Emacs blog [Emacs Redux](https://emacsredux.com).

## crux and super-save

A lot of utility commands that used to be part of Prelude were eventually extracted to the [crux](https://github.com/bbatsov/crux) package,
so they'd be easily available to more people.
These days Prelude simply depends on that package.

The [super-save](https://github.com/bbatsov/super-save) package also used to be part of Prelude in the past.

## Known issues

Check out the project's
[issue list](https://github.com/bbatsov/prelude/issues?sort=created&direction=desc&state=open)
a list of unresolved issues. By the way - feel free to fix any of them
and send me a pull request. :-)

## Support

Support is available via several channels:

* Prelude's Google Group <emacs-prelude@googlegroups.com>
* Prelude's Freenode channel (`#prelude-emacs`)
* [Gitter](https://gitter.im/bbatsov/prelude)

## Contributors

Here's a [list](https://github.com/bbatsov/prelude/contributors) of all the people who have contributed to the
development of Emacs Prelude.

## Bugs & Improvements

Bug reports and suggestions for improvements are always
welcome. GitHub pull requests are even better! :-)

## License

Copyright © 2011-2023 Bozhidar Batsov and
[contributors](https://github.com/bbatsov/prelude/contributors).

Distributed under the GNU General Public License, version 3

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
