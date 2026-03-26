# Prelude Key Chord

## Overview

This module enables [key-chord-mode](https://www.emacswiki.org/emacs/KeyChord),
which allows you to bind commands to combinations of two keys pressed
simultaneously (or in quick succession).

## Default Key Chords

| Chord | Command | Description |
| ----- | ------- | ----------- |
| <kbd>jj</kbd> | `avy-goto-word-1` | Jump to the beginning of a word |
| <kbd>jl</kbd> | `avy-goto-line` | Jump to a visible line |
| <kbd>jk</kbd> | `avy-goto-char` | Jump to a visible character |
| <kbd>JJ</kbd> | `crux-switch-to-previous-buffer` | Switch to previous buffer |
| <kbd>uu</kbd> | `undo-tree-visualize` | Visualize the undo tree |
| <kbd>xx</kbd> | `execute-extended-command` | Same as M-x |
| <kbd>yy</kbd> | `browse-kill-ring` | Browse the kill ring |

## Adding Your Own Key Chords

You can define additional key chords in your personal config:

```emacs-lisp
(key-chord-define-global "FF" 'find-file)
```
