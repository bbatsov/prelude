# Contributing

## Issues

Report issues and suggest features and improvements on the
[GitHub issue tracker](https://github.com/bbatsov/prelude/issues). Don't ask
questions on the issue tracker - use the [support channels](support.md) instead.

If you want to file a bug, please provide all the necessary info listed in
our issue reporting template (it's loaded automatically when you create a
new GitHub issue).

## Patches

Patches in any form are always welcome! GitHub pull requests are even better! :-)

Before submitting a patch or a pull request make sure that your patch
is in line with the [contribution
guidelines](https://github.com/bbatsov/prelude/blob/master/CONTRIBUTING.md).

## Documentation

Good documentation is just as important as good code.
Please, consider improving and extending this manual.

### Working on the Manual

The manual is generated from the markdown files in the
[doc](https://github.com/bbatsov/prelude/tree/master/doc) folder of Prelude's
GitHub repo and is published to [Read the Docs](readthedocs.org). The
[MkDocs](http://www.mkdocs.org/) tool is used to convert the markdown sources to
HTML.

To make changes to the manual you simply have to change the files under
`doc`. The manual will be regenerated automatically when changes to those files
are merged in `master` (or the latest stable branch).

You can install `MkDocs` locally and use the command `mkdocs serve` to see the
result of changes you make to the manual locally:

```sh
$ cd path/to/prelude/repo
$ mkdocs serve
```

If you want to make changes to the manual's page structure you'll have to edit
[mkdocs.yml](https://github.com/bbatsov/prelude/blob/master/mkdocs.yml).

## Donations

You can support the development of Prelude via
[GitHub Sponsors](https://github.com/sponsors/bbatsov),
[PayPal](https://www.paypal.me/bbatsov) and
[Patreon](https://www.patreon.com/bbatsov).
