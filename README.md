[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Liberapay](https://liberapay.com/assets/widgets/donate.svg)](https://liberapay.com/bbatsov/donate)
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
**26.1**.

You can support the development of Prelude via
[PayPal](https://www.paypal.me/bbatsov),
[Salt](https://bountysource.com/teams/prelude),
[Patreon](https://www.patreon.com/bbatsov) and
[Liberapay](https://liberapay.com/bbatsov/donate).

[![Liberapay](https://liberapay.com/assets/widgets/donate.svg)](https://liberapay.com/bbatsov/donate)
[![Patreon](https://img.shields.io/badge/patreon-donate-orange.svg)](https://www.patreon.com/bbatsov)

## Fast Forward

Assuming you're using an Unix-like OS (`*BSD`, `GNU/Linux`, `macOS`, `Solaris`,
etc), you already have Emacs 24.4+ installed, as well as `git` & `curl` you
can skip the whole manual and just type in your favorite shell the
following command:

```bash
curl -L https://git.io/epre | sh
```

You can now power up your Emacs, sit back and enjoy Prelude,
forgetting about the rest of this manual.

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

Don't forget to adjust your `prelude-modules.el` file in your personal directory
once the installation is done. By default most of the modules
that ship with Prelude are not loaded.

## User Manual

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

Cheers,<br/>
[Bozhidar](https://twitter.com/bbatsov)

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
