# Prelude Ivy

## Overview

[Ivy](https://github.com/abo-abo/swiper) is a generic completion framework for
Emacs, similar to Ido and Helm but lighter. This module sets up Ivy along with
its companion packages Swiper and Counsel.

!!! Note

    This module is an alternative to the [Helm](helm.md), [Ido](ido.md),
    [Vertico](vertico.md) modules - you should
    only enable one completion framework.

## Packages

- [ivy](https://github.com/abo-abo/swiper) - the completion framework
- [swiper](https://github.com/abo-abo/swiper) - Ivy-powered
  enhanced buffer search
- [counsel](https://github.com/abo-abo/swiper) - Ivy-powered
  replacements for common Emacs commands

## Key Bindings

### Ivy

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>C-c C-r</kbd> | `ivy-resume` | Resume the last Ivy completion session |
| <kbd>F6</kbd> | `ivy-resume` | Same as above |

### Swiper

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>C-s</kbd> | `swiper` | Ivy-powered buffer search |

### Counsel

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>M-x</kbd> | `counsel-M-x` | Ivy-powered command execution |
| <kbd>C-x C-f</kbd> | `counsel-find-file` | Ivy-powered file finding |
| <kbd>F1 f</kbd> | `counsel-describe-function` | Describe function |
| <kbd>F1 v</kbd> | `counsel-describe-variable` | Describe variable |
| <kbd>F1 l</kbd> | `counsel-find-library` | Find Emacs library |
| <kbd>F2 i</kbd> | `counsel-info-lookup-symbol` | Info lookup |
| <kbd>F2 u</kbd> | `counsel-unicode-char` | Insert Unicode character |
| <kbd>C-c g</kbd> | `counsel-git` | Find file in current Git repo |
| <kbd>C-c j</kbd> | `counsel-git-grep` | Grep in current Git repo |
| <kbd>C-c a</kbd> | `counsel-ag` | Search with ag (the silver searcher) |
| <kbd>C-x l</kbd> | `counsel-locate` | Locate file |
| <kbd>M-y</kbd> | `counsel-yank-pop` | Browse kill ring |
