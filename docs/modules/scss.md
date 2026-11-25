# Prelude SCSS

!!! Note

    This module builds on top of the [CSS](css.md) module.

## SCSS Mode

Emacs comes with a `scss-mode` for editing SCSS (Sassy CSS) files, and
that's what the module uses. It's a child of `css-mode`, so it gets
all the CSS module's defaults:

- **Rainbow mode** for colorizing color strings
- **2-space indentation**
- A language server, when there's one installed
  (`vscode-css-language-server` handles SCSS as well)

If you need something set up differently for SCSS, add a function to
`scss-mode-hook`.

!!! Note

    Prelude used to install the `scss-mode` package from MELPA. It
    hasn't been updated in years and no longer loads in a modern
    Emacs, which left SCSS files in `fundamental-mode`. The module
    makes sure that the built-in mode is used even if you still have
    the package installed, but you might as well delete it
    (<kbd>M-x package-delete</kbd>).
