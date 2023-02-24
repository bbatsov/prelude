# Installation

## Prerequisites

Obviously to use the Emacs Prelude you have to install Emacs
first. We'll assume you can manage this part on your own.
Aim for the newest stable Emacs release, although as a rule of
thumb Prelude aims to support the last 2-3 stable releases.

For spell-checking to work you should install `aspell`, together with its
dictionaries for the languages you wish to check.

You'll also do well to install some of the following:

* `git` (needed by Magit)
* `ag` (`the_silver_searcher`) or `ripgrep` (Projectile has nice integration with them and they are much faster than `grep`)
* your favorite lint tools (for Flycheck)

All those tools are completely optional, though.

!!! Note

    Additional external tools might be needed by some of the modules (e.g. tools specific to particular programming languages, etc).

## Installing Prelude

### Automated

You can install Emacs Prelude via the command line with either `curl` or
`wget`. Naturally `git` is also required.

The installer script will do the following:

* Clone Prelude's GitHub repo
* Check your Emacs version
* Backup any existing `.emacs` or `.emacs.d` you might have
* Create any additional folders if necessary (e.g. for storing
  package-specific data)

If you have a `.emacs` file it will backed up as `.emacs.pre-prelude`
and if you have a `.emacs.d` folder, it will be backed up as
`.emacs.d.pre-prelude.tar`.

#### Via Curl

If you're using `curl` type the following command:

```shellsession
$ curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh | sh
```

#### Via Wget

If you're using `wget` type:

```shellsession
$ wget --no-check-certificate https://github.com/bbatsov/prelude/raw/master/utils/installer.sh -O - | sh
```

### Manual

Make sure you do not have any `~/.emacs` file or `~/.emacs.d` folder
present.

```shellsession
$ git clone https://github.com/bbatsov/prelude.git path/to/local/repo
$ ln -s path/to/local/repo ~/.emacs.d
$ cd ~/.emacs.d
```

!!! Note

    If you are using Windows, you should check what Emacs thinks the `~` directory is by running Emacs
    and typing `C-x d ~/<RET>`, and then adjust the command appropriately.

### System-wide (site-wide)

For a multi-user environment, as an admin, the customizations intended
for all users go in the site-start file represented by the variable
`site-run-file`, while single users will use their own init file
represented by the variable `user-init-file`.

If you have placed your Prelude directory in `/opt/prelude` then,
append the following line to the `site-start.el`:

``` emacs-lisp
(load "/opt/prelude/init.el")
```

If you are using Emacs as a daemon process, with other users or daemon
processes interacting with the Emacs daemon (e.g. Emacs is your window
manager) then the `site-lisp` directory could be the right place to
place your configuration files.

## Pinning packages

By default, Prelude will install packages from the MELPA and GNU ELPA
package repositories. Occasionally package integration can break when
upgrading packages, as the packages in the MELPA repository are all
snapshot builds.  This can be avoided by pinning packages to stable
versions in other repositories (e.g. MELPA Stable).  To do so, copy
`prelude-pinned-packages.el` from the sample directory to Prelude's
root directory and adjust the
[variables](https://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Installation.html)
inside accordingly.

## Enabling additional modules

By default most of the modules that ship with Prelude are not
loaded. For more information on the functionality provided by these
modules visit the [docs](modules/index.md).

```lisp
;;; Uncomment the modules you'd like to use and restart Prelude afterwards

;;; General productivity tools

;; (require 'prelude-ido) ;; Supercharges Emacs completion for C-x C-f and more
;; (require 'prelude-ivy) ;; A mighty modern alternative to ido
(require 'prelude-vertico) ;; A powerful, yet simple, alternative to ivy
;; (require 'prelude-helm) ;; Interface for narrowing and search
;; (require 'prelude-helm-everywhere) ;; Enable Helm everywhere
(require 'prelude-company)
;; (require 'prelude-key-chord) ;; Binds useful features to key combinations

;;; Vim emulation
;;
;; Enable this module if you're fond of vim's keybindings.
;; (require 'prelude-evil)

;;; Org-mode (a legendary productivity tool that deserves its own category)
;;
;; Org-mode helps you keep TODO lists, notes and more.
(require 'prelude-org)

;;; Programming languages support
;;
;; Modules for a few very common programming languages
;; are enabled by default.

(require 'prelude-c)
;; (require 'prelude-clojure)
;; (require 'prelude-coffee)
;; (require 'prelude-common-lisp)
(require 'prelude-css)
;; (require 'prelude-dart)
(require 'prelude-emacs-lisp)
;; (require 'prelude-erlang)
;; (require 'prelude-elixir)
;; (require 'prelude-go)
;; (require 'prelude-haskell)
(require 'prelude-js)
;; (require 'prelude-latex)
(require 'prelude-lisp) ;; Common setup for Lisp-like languages
(require 'prelude-lsp) ;; Base setup for the Language Server Protocol
;; (require 'prelude-lua)
;; (require 'prelude-ocaml)
(require 'prelude-perl)
;; (require 'prelude-python)
;; (require 'prelude-racket)
;; (require 'prelude-ruby)
;; (require 'prelude-rust)
;; (require 'prelude-scala)
;; (require 'prelude-scheme)
(require 'prelude-shell)
;; (require 'prelude-scss)
;; (require 'prelude-ts)
(require 'prelude-web) ;; Emacs mode for web templates
(require 'prelude-xml)
(require 'prelude-yaml)

;;; Misc
(require 'prelude-erc) ;; A popular Emacs IRC client (useful if you're still into Freenode)
```

You'll need to adjust your `prelude-modules.el` file once the
installation is done.

In case of an automated installation, you'll find this file in the
`personal` directory of your Emacs installation.

If you are doing a manual install then you first need to copy the
`prelude-modules.el` available in the sample directory to the root of
`path/to/prelude/installation` and then adjust that one.

After you've uncommented a module you should either restart Emacs or
evaluate the module `require` expression with <kbd>C-x C-e</kbd>.

## Updating Prelude

### Automatic update

Simply run <kbd>M-x prelude-update</kbd> from Emacs itself and restart
Emacs afterwards.

### Manual update

The update procedure is fairly straightforward and consists of 3
steps:

#### Update all bundled packages

Just run <kbd>M-x package-list-packages RET U x</kbd>.

!!! Note

    Technically speaking, this will update all the packages you've installed,
    not just those that were bundled with Prelude. That's fine most of the time.

#### Update Prelude's code

```shellsession
$ cd path/to/prelude/installation
$ git pull
```

The `path/to/prelude/installation` is usually `~/.emacs.d` (at least
on Unix systems).

#### Restart Prelude

It's generally a good idea to stop Emacs after you do the update. The
next time Prelude starts it will install any new dependencies (if
there are such).

## Uninstalling Prelude

Provided you've installed Prelude in `.emacs.d`, all you need to do is
delete that folder.  If you opted for the manual installation and
making `.emacs.d` a symlink - you remove/update the link. Yeah, it's
as simple as that. No fancy uninstaller required!
