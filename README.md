[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Patreon](https://img.shields.io/badge/patreon-donate-orange.svg)](https://www.patreon.com/bbatsov)

Emacs Prelude
=============

Prelude is an Emacs distribution that aims to enhance the default
Emacs experience.  Prelude alters a lot of the default settings,
bundles a plethora of additional packages and adds its own core
library to the mix. The final product offers an easy to use Emacs
configuration for Emacs newcomers and lots of additional power for
Emacs power users.

Prelude is compatible **ONLY with GNU Emacs 25.1+**. In general you're
advised to always run Prelude with the latest Emacs - currently
**26.3**.

You can support the development of Prelude via
[PayPal](https://www.paypal.me/bbatsov) and
[Patreon](https://www.patreon.com/bbatsov).

## Fast Forward

Assuming you're using an Unix-like OS (`*BSD`, `GNU/Linux`, `macOS`, `Solaris`,
etc), you already have a recent version of Emacs installed, as well as `git` & `curl` you
can skip the whole manual and just type in your favorite shell the
following command:

```bash
curl -L https://git.io/epre | sh
```

You can now power up your Emacs, sit back and enjoy Prelude.

There are two environment variables you can use to control the
source repository and the installation directory. To change the
installation directory:

```bash
export PRELUDE_INSTALL_DIR="$HOME/.emacs.d" && curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh | sh
```

To change the source repository:

```bash
export PRELUDE_URL="https://github.com/yourname/prelude.git" && curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh | sh
```

Note that the installer will back up any existing `.emacs` file or
`.emacs.d` since it will unpack Prelude's code in `.emacs.d`. If
you're doing a manual install make sure you don't have a `.emacs` file
or back up your existing `.emacs.d` directory manually.

**Important:** Don't forget to adjust your `prelude-modules.el` file in your personal directory
once the installation is done. By default most of the modules
that ship with Prelude are **not** loaded.

## User Manual

While Emacs Prelude is pretty simple at its core, it does have some specifics that
are worth learning - e.g. configuration options, load order of modules and personal settings
and so on.

Check out our [user manual](https://prelude.emacsredux.com) for more information.

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

Copyright © 2011-2020 Bozhidar Batsov and
[contributors](https://github.com/bbatsov/prelude/contributors).

Distributed under the GNU General Public License, version 3

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
