# Writing a Module

This section is dedicated to people who are writing new modules or
changing existing ones. Most of Prelude's modules set up a programming
language, so that's the focus here, but just about everything applies
to the other modules as well. It also helps to look at a couple of the
existing modules (e.g. `prelude-csharp`, `prelude-ruby` and
`prelude-go`).

## Just Emacs Lisp

Prelude should never feel alien to people who know Emacs. A module is
meant to look like something you could have written in your own init
file: a few `use-package` forms, a function on the hooks of a major
mode, some options and key bindings. There are no macros for defining
modules, no layers and no configuration language of our own, and there
shouldn't be any. If you know how to configure Emacs, you already know
how to change what a module does. The same goes for its users, who
customize what Prelude set up the same way they would customize stock
Emacs (with the hooks of the major modes and the options of the
packages).

That's why modules stick to the standard ways of doing things in Emacs
Lisp: mode hooks, keymaps, `with-eval-after-load`, plain variables and
`defcustom`. Prelude has a handful of helpers of its own, because they
save a lot of work in every module (e.g. picking the tree-sitter mode
and installing its grammar, or starting the right LSP client). They
are plain functions, too. Before adding another one, ask yourself
whether a user reading your module would have to look it up to
understand what's going on.

The same goes for packages. Users have to live with every package a
module installs, so prefer what comes with Emacs, pick third-party
packages that are well established and maintained, and keep the list
short. A module that sets up just the major mode and the language
server is a perfectly fine module.

## A typical language module

Here's what a module for a made up language called Example would look
like. Everything in it is explained below.

<!-- markdownlint-disable MD013 -->

``` emacs-lisp
;;; prelude-example.el --- Emacs Prelude: Example programming support.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Example support: example-ts-mode, or example-mode when there's no
;; Example grammar, and the Example language server.

;;; License:

;; (the GPL notice, the same as in every other module)

;;; Code:

(require 'prelude-programming)

;; Use example-ts-mode when the tree-sitter grammar is available
(prelude-treesit-remap 'example 'example-mode 'example-ts-mode)

;; The classic mode, and the commands for running Example code
(use-package example-mode
  :ensure t
  :defer t
  :bind (:map example-mode-map
              ("C-c C-r" . example-run)))

;; The Example style guide says 4
(setq example-indent-offset 4)

(defun prelude-example-setup ()
  "Set up an Example buffer."
  ;; CamelCase aware editing operations
  (subword-mode +1)
  (prelude-lsp-enable))

(add-hook 'example-mode-hook #'prelude-example-setup -50)
(add-hook 'example-ts-mode-hook #'prelude-example-setup -50)

(provide 'prelude-example)
;;; prelude-example.el ends here
```

<!-- markdownlint-enable MD013 -->

### The first and the last lines

The first line names the file, says what it is and turns on
`lexical-binding`, which every module needs. The commentary says what
the module sets up, in a sentence or two. The last two lines are always
the `provide` and the `ends here` comment. The test suite checks all of
that, so don't get creative. If you wrote the module, the copyright and
the author lines are yours, of course.

### The foundation

A language module starts with `(require 'prelude-programming)` (or
`prelude-lisp` for a Lisp). That's where the things that every
programming mode gets come from: highlighting of whitespace problems,
spell checking in comments, automatic pairing of delimiters, etc.
(they are in `prelude-prog-setup`, on `prog-mode-hook`). This means the
module only has to add what's specific to its language. A Lisp module
also calls `prelude-lisp-coding-setup` from its setup, and
`prelude-interactive-lisp-coding-setup` from the setup of its REPL
buffers.

### Packages

Install packages with `use-package` and `:ensure t`. Prelude keeps track
of the packages installed that way, and `M-x prelude-update-packages`
updates them. Don't call `package-install` yourself.

Nothing should be loaded at startup unless it really has to be, as
everyone who uses the module pays for it on every start. Give
`use-package` a `:defer t` (or `:bind`, `:mode`, `:hook` or
`:commands`, which defer the loading as well). Keep in mind that a
plain `:ensure t` loads the package right away.

When a package is only needed with one of the LSP clients, install it
only for that one:

``` emacs-lisp
(when (eq prelude-lsp-client 'eglot)
  (use-package eglot-java
    :ensure t
    :defer t))
```

### Major modes and tree-sitter

When Emacs has both a classic mode and a tree-sitter mode for the
language, `prelude-treesit-remap` switches to the tree-sitter mode when
the grammar is installed, and offers to install it otherwise. When
there's only a tree-sitter mode, map the file names to it with
`prelude-treesit-auto-mode`, which takes a fallback mode for when the
grammar is missing. The grammars Prelude installs are pinned in
`prelude-treesit-language-sources`. Add yours there if Emacs doesn't
know where to get it.

The [Design Notes](index.md#design-notes) explain which major mode the
modules prefer and why.

### Setting up the buffers

What a module does in the buffers of its language goes into one
function with a docstring, `prelude-example-setup` in our case. It goes
on the hooks of every mode it's meant for, and that's usually both the
classic mode and the tree-sitter mode. Note that they don't share their
hooks (e.g. `python-ts-mode` doesn't run `python-mode-hook`).

There are a few rules here, all of them about not getting in the way of
the users:

- The function goes on the hooks with a depth of -50. The functions
  people add to the same hooks in their config (at the default depth of
  0) run after it, so they can change anything it did, and nobody has to
  think about the order.
- It's a named function, never a lambda. The [Emacs Lisp
  manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/Setting-Hooks.html)
  advises against lambdas on hooks. A named function can be removed
  from a hook, shows up as something readable in <kbd>C-h v</kbd> and
  has a docstring. If you don't want Prelude's setup of a mode, you
  simply remove it from the hooks. The tests check that no module puts
  a lambda on a hook.
- It's added with `add-hook`, not with the `:hook` of `use-package`,
  which can't take a depth.
- It only does what's local to the buffer: it turns on minor modes, sets
  buffer-local variables with `setq-local`, adds functions to
  buffer-local hooks (e.g.
  `(add-hook 'before-save-hook #'some-function nil t)`) and starts the
  language server.

Keep an eye on modes that are built on other modes. Their buffers run
the hooks of the parent modes too, so `scss-mode` buffers get the setup
of `css-mode` for free. Sometimes the setup of the parent mode has to
stay out of its children, though. On Emacs 29 `js-json-mode` is built
on `js-mode`, for instance, and `prelude-js-setup` leaves JSON buffers
to `prelude-json-setup`.

The modules that were around in Prelude 2.1 also run an obsolete
`prelude-example-mode-hook` at the end of their setup, for the personal
configs that still use it. New modules don't need one.

### Options

Options that aren't local to a buffer are set with `setq` at the top
level of the module, like `example-indent-offset` above. The module is
loaded before the personal config, so whatever the user sets there (or
with Customize) wins. A `defcustom` keeps the value that a variable had
before its package got loaded, so it's fine to set the variable before
that. The `:custom` of `use-package` does the job as well, but keep in
mind that in a `use-package` with `:after` it runs only once the other
package is loaded (`:init` too), and that's usually after the personal
config. The tests check that no module does that. Also, `:custom` works
only for real options: a variable that the package makes with `defvar`
(e.g. the cursors of Evil's states) has to be set with `setq` in
`:init`.

Don't set such options in the setup function, as that would undo the
user's setting every time a buffer is opened. Don't set them in a
`with-eval-after-load` either, as the package usually gets loaded after
the personal config, with the same result. `with-eval-after-load` is
the place for things that need the package itself (e.g. changing one of
its faces), which then happen once, when the package is loaded.

Add an option of Prelude's own (a `defcustom` named `prelude-...`, in
the `prelude` group) only when users really need a setting that the
packages don't give them. An option that decides what a module installs
(e.g. `prelude-lsp-client`) has to be set in `personal/preload`, before
the module is loaded, so mention that in its documentation.

### Key bindings

Bind keys in the keymap of the mode (with `:bind` and `:map` in
`use-package`, or with `define-key` in a `with-eval-after-load`). Keep
in mind that a tree-sitter mode often has a keymap of its own (e.g.
`go-ts-mode-map`). Follow the [key binding
conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html)
of Emacs. <kbd>C-c</kbd> followed by a letter is reserved for users, so
a module never binds it, but <kbd>C-c</kbd> followed by a control
character is fair game in the keymap of a major mode. Don't take away
keys that people rely on in stock Emacs, and prefer the bindings that
the mode already has.

### Language servers

Call `prelude-lsp-enable` from the setup. It starts Eglot or lsp-mode,
whichever the user picked (`prelude-lsp-client`). With Eglot it does
nothing when the server isn't installed, apart from saying so once.
Give it a non-nil argument (`(prelude-lsp-enable 'quiet)`) in modules
for the kinds of files that people open all the time without needing a
language server for them (e.g. configuration files).

Prelude doesn't install language servers, so don't do that in a module
(unless the server can only be installed through a package, like the one
for Java). When Eglot doesn't know the server of a language, you can
tell it about the server like this:

``` emacs-lisp
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(example-ts-mode . ("example-ls"))))
```

The Lisp modules don't start language servers at all. They are built
around a REPL connection, which does the same job and then some.

### Formatting on save

If the language server can format the code, add
`prelude-lsp-format-on-save` to `before-save-hook` (locally) in the
setup. If something else formats it, check `prelude-format-on-save-p`
first. It respects `prelude-format-on-save` and leaves the job to
Apheleia when that's enabled. See `prelude-go` for an example.

### Byte compilation

Every module has to compile without warnings, and CI checks that. A
module uses plenty of variables and functions from packages that are
not loaded yet when it's compiled, so declare them with `defvar` and
`declare-function`. Don't silence warnings without a comment saying
why. `M-x checkdoc` (<kbd>C-c ? d</kbd>) should be happy with the
docstrings too.

## Tests

The tests live in `test/prelude-test.el` and use ERT. When you add a
module, add its setup function and the hooks it goes on to
`prelude-test--setups`. Then add tests for what's specific to the
module (e.g. what the setup turns on, the key bindings, the file names
it handles). CI loads every module (`test/prelude-load-test.el`), so a
module that can't be loaded together with the others has to be added
to the exceptions there. Such modules (e.g. the completion modules) are
loaded one by one in CI instead, each with the tests in
`test/prelude-standalone-test.el`. You can run the tests like this:

``` shell
emacs --batch -l init.el -l test/prelude-test.el -f ert-run-tests-batch-and-exit
emacs --batch -l init.el -l test/prelude-compile-test.el
```

## Documentation

A module needs a page in `docs/modules/`, linked from `mkdocs.yml` and
from the [list of modules](index.md). Check out the pages of other
modules for what goes in one: which modules it builds on, what has to
be installed on the system (e.g. compilers and language servers), the
major mode and what Prelude sets up in its buffers, how to change that,
and a table of the key bindings. Add the module to
`sample/prelude-modules.el` as well (commented out, unless it's meant
to be on by default), and add an entry to the changelog.

## In a nutshell

- The first and the last lines look like the ones in the other modules.
- Packages are installed with `use-package` and `:ensure t`, and
  loaded only when needed.
- The setup of the buffers is in `prelude-NAME-setup`, on the hooks
  of the classic and the tree-sitter mode, with a depth of -50.
- No lambdas on hooks, and no global options set in the setup.
- Nothing is bound under <kbd>C-c</kbd> followed by a letter.
- It compiles without warnings.
- Tests, a page in the manual, an entry in
  `sample/prelude-modules.el` and in the changelog.
