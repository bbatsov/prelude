# Prelude SCSS

!!! Note

    This module builds on top of the [CSS](css.md) module.

## SCSS Mode

This module bundles [scss-mode](https://github.com/antonj/scss-mode) for editing
SCSS (Sassy CSS) files.

It inherits all the CSS module defaults, including:

- **Rainbow mode** for colorizing color strings
- **2-space indentation**
- The `prelude-prog-mode-hook` features

The automatic compile-on-save feature of `scss-mode` is
**disabled** by default, as it can be disruptive. If you want
to enable it, add to your personal config:

```emacs-lisp
(setq scss-compile-at-save t)
```
