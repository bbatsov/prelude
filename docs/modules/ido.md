# Prelude Ido

## Overview

[Ido](https://www.gnu.org/software/emacs/manual/html_mono/ido.html)
(Interactively DO things) is a built-in Emacs package for
interactive buffer switching and file finding. This module enhances Ido with
better fuzzy matching and additional features.

!!! Note

    This module is an alternative to the [Helm](helm.md), [Ivy](ivy.md),
    [Vertico](vertico.md) modules - you should
    only enable one completion framework.

## Packages

The following packages are installed:

- [flx-ido](https://github.com/lewang/flx) - advanced fuzzy
  matching for Ido
- [ido-completing-read+][ido-cr+] - use Ido for even more
  completing-read calls
- [smex](https://github.com/nonsequitur/smex) - a smart M-x
  enhancement (Ido-powered, with history and frequency sorting)

[ido-cr+]: https://github.com/DarwinAwardWinner/ido-completing-read-plus

## Configuration

Prelude sets the following Ido defaults:

- Flex matching enabled
- No prefix matching required
- Creates new buffers without confirmation
- Uses filename at point as a guess
- Shows up to 10 completion prospects
- Opens files in the selected window

## Key Bindings

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>M-x</kbd> | `smex` | Ido-powered command execution |
| <kbd>M-X</kbd> | `smex-major-mode-commands` | Major mode commands |
