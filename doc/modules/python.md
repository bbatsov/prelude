# Prelude Python

!!! Note

    This module builds on top of the shared [Programming](programming.md) module.

## Python Mode

Emacs comes with Python programming support through the built-in
`python-mode`. Whenever you are editing Python code run `C-h m` to
look at the Python mode key bindings. Alternatively look at the
menu bar entries under Python. To toggle the menu bar press `F12`.

## Anaconda Mode

Prelude bundles the powerful
[anaconda-mode](https://github.com/pythonic-emacs/anaconda-mode),
which provides code navigation, documentation lookup and completion for Python.

Anaconda has integration with popular modes like `company` and `eldoc`.

## Syntax checking

Prelude ships with [Flycheck](https://github.com/flycheck/flycheck),
an on the fly syntax checker. Flycheck has support for two Python
syntax checkers, [Pylint](http://www.pylint.org/) and
[Flake8](http://flake8.readthedocs.org/en/latest/). In
order to have Flycheck support on the fly syntax checking for
Python you need to have either of these installed and accessible to
Emacs. In order to manually choose a checker run `C-c ! s`.

## Automatic insertion of file encoding comments

You can have Prelude auto-detect the encoding of a source buffer and
insert the appropriate `# coding:` comments.  If you wish to enable
this, add the following to your configuration:

```emacs-lisp
(setq prelude-python-mode-set-encoding-automatically t)
```

!!! Note

    Previously `prelude-python` had this feature enabled by default (up to Prelude 1.1), but
    it is only necessary on Python 2, because Python 3 uses utf-8
    as the default file encoding. In 2020 Python 2 became deprecated, so that
    functionality became mostly obsolete.
