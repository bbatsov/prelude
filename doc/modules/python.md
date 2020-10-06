# Prelude Python

!!! Note

    This module builds on top of the shared [Programming](programming.md) module.

## Python Mode

Emacs comes with Python programming support through the built-in
`python-mode`. Whenever you are editing Python code run `C-h m` to
look at the Python mode key bindings. Alternatively look at the
menu bar entries under Python. To toggle the menu bar press `F12`.

## Syntax checking

Prelude ships with [Flycheck](https://github.com/flycheck/flycheck),
an on the fly syntax checker. Flycheck has support for two Python
syntax checkers, [Pylint](http://www.pylint.org/) and
[Flake8](http://flake8.readthedocs.org/en/latest/). In
order to have Flycheck support on the fly syntax checking for
Python you need to have either of these installed and accessible to
Emacs. In order to manually choose a checker run `C-c ! s`.


## Automatic insertion of # coding: utf-8

Previously `prelude-python` had this feature enabled by default, but
that is only necessary on Python2, because Python3 already use utf-8
as default encoding. In 2020, python2 becames deprecated, so that
functionallity becames a annoying side-effect for some users. If you
wish to enable this, add this to your config file:

```emacs-lisp
(setq prelude-python-mode-set-encoding-automatically t)
```
