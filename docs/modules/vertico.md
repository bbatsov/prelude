# Prelude Vertico

## Overview

[Vertico](https://github.com/minad/vertico) is a minimalistic vertical
completion UI for Emacs. This module sets up Vertico together with complementary
packages for a modern and lightweight completion experience.

!!! Note

    This module is an alternative to the [Helm](helm.md), [Ido](ido.md),
    [Ivy](ivy.md) modules - you should
    only enable one completion framework.

## Packages

- [vertico](https://github.com/minad/vertico) - the vertical completion UI
- [orderless](https://github.com/oantolin/orderless) - flexible
  completion style (space-separated patterns)
- [consult](https://github.com/minad/consult) - enhanced versions
  of built-in commands

## Consult Key Bindings

Consult provides many enhanced commands. Here are the key bindings configured by
Prelude:

### Buffer and File Navigation

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>C-x b</kbd> | `consult-buffer` | Enhanced buffer switching |
| <kbd>C-x 4 b</kbd> | `consult-buffer-other-window` | Buffer in other window |
| <kbd>C-x 5 b</kbd> | `consult-buffer-other-frame` | Buffer in other frame |
| <kbd>M-y</kbd> | `consult-yank-pop` | Browse kill ring |

### Navigation (M-g prefix)

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>M-g g</kbd> | `consult-goto-line` | Go to line |
| <kbd>M-g o</kbd> | `consult-outline` | Navigate outline headings |
| <kbd>M-g m</kbd> | `consult-mark` | Navigate marks |
| <kbd>M-g k</kbd> | `consult-global-mark` | Navigate global marks |
| <kbd>M-g i</kbd> | `consult-imenu` | Navigate imenu |
| <kbd>M-g I</kbd> | `consult-imenu-multi` | Navigate imenu across buffers |
| <kbd>M-g e</kbd> | `consult-compile-error` | Navigate compilation errors |
| <kbd>M-g f</kbd> | `consult-flycheck` | Navigate Flycheck errors |

### Search (M-s prefix)

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>M-s l</kbd> | `consult-line` | Search lines in buffer |
| <kbd>M-s L</kbd> | `consult-line-multi` | Search lines across buffers |
| <kbd>M-s g</kbd> | `consult-grep` | Grep |
| <kbd>M-s G</kbd> | `consult-git-grep` | Git grep |
| <kbd>M-s r</kbd> | `consult-ripgrep` | Ripgrep |
| <kbd>M-s f</kbd> | `consult-find` | Find files |
| <kbd>M-s F</kbd> | `consult-locate` | Locate files |
| <kbd>M-s k</kbd> | `consult-keep-lines` | Keep matching lines |
| <kbd>M-s u</kbd> | `consult-focus-lines` | Focus on matching lines |

### Registers

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>M-#</kbd> | `consult-register-load` | Load register |
| <kbd>M-'</kbd> | `consult-register-store` | Store register |
| <kbd>C-M-#</kbd> | `consult-register` | Browse registers |

## Completion Style

The module configures the `orderless` completion style, which allows you to
type space-separated patterns that can match in any order. For example, typing
`buf swi` would match `switch-to-buffer`.
