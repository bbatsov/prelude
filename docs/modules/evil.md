# Prelude Evil

## Overview

This module provides Vim emulation in Emacs via
[evil-mode](https://github.com/emacs-evil/evil).
It bundles several useful evil extensions and sets up sensible
defaults for a Vim-like experience within the Prelude
environment.

## Packages

The following packages are installed and configured:

- [evil](https://github.com/emacs-evil/evil) - the extensible vi layer for Emacs
- [evil-surround](https://github.com/emacs-evil/evil-surround) -
  emulates
  [vim-surround](https://github.com/tpope/vim-surround)
- [evil-visualstar](https://github.com/bling/evil-visualstar) -
  search visual selection with `*`
- [evil-numbers](https://github.com/cofi/evil-numbers) -
  increment/decrement numbers like in Vim
- [goto-chg](https://github.com/emacs-evil/goto-chg) - go to
  recent changes (`g-;` and `g-,`)

## Key Bindings

### Normal State

<!-- markdownlint-disable MD013 -->

| Key              | Command                        | Description                 |
| ---------------- | ------------------------------ | --------------------------- |
| <kbd>C-a</kbd>   | `evil-numbers/inc-at-pt`       | Increment number at point   |
| <kbd>C-S-a</kbd> | `evil-numbers/dec-at-pt`       | Decrement number at point   |
| <kbd>Y</kbd>     | `prelude-yank-to-end-of-line`  | Yank to end of line         |
| <kbd>SPC</kbd>   | `avy-goto-word-1`              | Jump to a word using Avy    |
| <kbd>C-S-d</kbd> | Scroll down other window       |                             |
| <kbd>C-S-u</kbd> | Scroll up other window         |                             |

<!-- markdownlint-enable MD013 -->

### Visual State

| Key            | Command     | Description                          |
| -------------- | ----------- | ------------------------------------ |
| <kbd>></kbd>   | Shift right | Shift right and restore selection    |
| <kbd><</kbd>   | Shift left  | Shift left and restore selection     |

### Ex Commands

| Command   | Action              |
| --------- | ------------------- |
| `:W`      | Write all buffers   |
| `:Tree`   | Open Speedbar       |
| `:linum`  | Toggle line numbers |
| `:Align`  | Align by regexp     |

## Org Mode Integration

The module includes custom evil key bindings for `org-mode`:

| Key | Command |
| --- | ------- |
| <kbd>gk</kbd> | `outline-up-heading` |
| <kbd>gj</kbd> | `outline-next-visible-heading` |
| <kbd>t</kbd> | `org-todo` |
| <kbd>,c</kbd> | `org-cycle` |
| <kbd>,e</kbd> | `org-export-dispatch` |
| <kbd>,n</kbd> / <kbd>,p</kbd> | Next/previous visible heading |
| <kbd>,t</kbd> | `org-set-tags-command` |
| <kbd>,u</kbd> | `outline-up-heading` |

## Magit Integration

HJKL navigation is added to Magit buffers (log, commit, branch manager, status).

## Configuration

The default cursor styles are:

- **Emacs state**: red box
- **Normal state**: gray box
- **Visual state**: gray box
- **Insert state**: gray bar
- **Motion state**: gray box

The default shift width is set to 2. You can customize these
in your personal config.
