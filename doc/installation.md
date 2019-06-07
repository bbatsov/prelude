# Installation

## Installing Emacs

Obviously to use the Emacs Prelude you have to install Emacs
first. Have a look at
the
[WikEmacs articles on installing Emacs](http://wikemacs.org/index.php/Installing_Emacs).

## Installation

### Automated

You can install **Emacs Prelude** via the command line with either `curl` or
`wget`. Naturally `git` is also required.

#### Via Curl

If you're using `curl` type the following command:

```bash
curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh | sh
```

#### Via Wget

If you're using `wget` type:

```bash
wget --no-check-certificate https://github.com/bbatsov/prelude/raw/master/utils/installer.sh -O - | sh
```

### Manual

Make sure you do not have any `~/.emacs` file present.

```bash
git clone git://github.com/bbatsov/prelude.git path/to/local/repo
ln -s path/to/local/repo ~/.emacs.d
cd ~/.emacs.d
```

If you are using Windows, you should check what Emacs thinks the `~` directory is by running Emacs and typing `C-x d ~/<RET>`, and then adjust the command appropriately.

## Updating Prelude

### Manual update

The update procedure is fairly straightforward and consists of 3 steps:

#### Update all bundled packages

Just run <kbd>M-x package-list-packages RET U x</kbd>.

#### Update Prelude's code

```bash
cd path/to/prelude/installation
git pull
```

The `path/to/prelude/installation` is usually `~/.emacs.d` (at least
on Unix systems).

#### Restart Prelude

It's generally a good idea to stop Emacs after you do the update. The
next time Prelude starts it will install any new dependencies (if
there are such).

### Automatic update

Simply run <kbd>M-x prelude-update</kbd> from Emacs itself and restart Emacs afterwards.

## Pinning packages

By default, Prelude will install packages from the melpa and gnu package
repositories. Occasionally package integration can break when upgrading packages.
This can be avoided by pinning packages to stable versions in other repositories.
To do so, copy `prelude-pinned-packages.el` from the sample directory to
Prelude's root directory and adjust the [variables](https://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Installation.html)
inside accordingly.

## Enabling additional modules

By default most of the modules that ship with Prelude are not loaded. For more information on the functionality provided by these modules visit the [docs](modules/index.md).

```lisp
;;; Uncomment the modules you'd like to use and restart Prelude afterwards

(require 'prelude-c)
;; (require 'prelude-clojure)
;; (require 'prelude-coffee)
;; (require 'prelude-common-lisp)
;; (require 'prelude-css)
(require 'prelude-emacs-lisp)
(require 'prelude-erc)
;; (require 'prelude-erlang)
;; (require 'prelude-elixir)
;; (require 'prelude-haskell)
(require 'prelude-js)
;; (require 'prelude-latex)
(require 'prelude-lisp)
(require 'prelude-org)
(require 'prelude-perl)
;; (require 'prelude-python)
;; (require 'prelude-ruby)
;; (require 'prelude-scala)
(require 'prelude-scheme)
;; (require 'prelude-scss)
;; (require 'prelude-web)
(require 'prelude-xml)
```

You'll need to adjust your `prelude-modules.el` file once the
installation is done. If you are doing a manual install then you first
need to copy the `prelude-modules.el` available in the sample
directory to the root of `path/to/prelude/installation` and then
adjust that one.

After you've uncommented a module you should either restart Emacs or evaluate the module
`require` expression with <kbd>C-x C-e</kbd>.
