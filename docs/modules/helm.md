# Prelude Helm

## Overview

[Helm](https://github.com/emacs-helm/helm) is an incremental completion and
narrowing framework for Emacs. This module sets up Helm with sensible defaults
and integrates it with Projectile.

!!! Note

    There are two Helm modules - `prelude-helm` (base setup) and
    `prelude-helm-everywhere` (replaces standard Emacs commands with Helm
    equivalents). You'll typically want to enable both. This module is an
    alternative to the [Ido](ido.md), [Ivy](ivy.md), [Vertico](vertico.md), and
    modules - you should only enable one
    completion framework.

## Base Configuration (prelude-helm)

The base module provides:

- **Fuzzy matching** for buffers
- **Helm Projectile** integration (when Projectile is enabled)
- **Google suggest** support via curl (when available)
- The command prefix is remapped from <kbd>C-x c</kbd> to
  <kbd>C-c h</kbd> (to avoid accidental Emacs exit)

### Key Bindings

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>C-c h</kbd> | `helm-command-prefix` | Helm command prefix |
| <kbd>C-c h o</kbd> | `helm-occur` | Search in current buffer |
| <kbd>C-c h g</kbd> | `helm-do-grep` | Grep with Helm |
| <kbd>C-c h C-c w</kbd> | `helm-wikipedia-suggest` | Wikipedia suggestions |
| <kbd>C-c h SPC</kbd> | `helm-all-mark-rings` | Browse mark rings |

## Helm Everywhere (prelude-helm-everywhere)

This module replaces many standard Emacs commands with Helm-powered equivalents:

| Key | Helm Command | Replaces |
| --- | ------------ | -------- |
| <kbd>M-x</kbd> | `helm-M-x` | `execute-extended-command` |
| <kbd>M-y</kbd> | `helm-show-kill-ring` | `yank-pop` |
| <kbd>C-x b</kbd> | `helm-mini` | `switch-to-buffer` |
| <kbd>C-x C-b</kbd> | `helm-buffers-list` | `list-buffers` |
| <kbd>C-x C-f</kbd> | `helm-find-files` | `find-file` |
| <kbd>C-h f</kbd> | `helm-apropos` | `describe-function` |
| <kbd>C-h r</kbd> | `helm-info-emacs` | `info-emacs-manual` |
| <kbd>C-c f</kbd> | `helm-recentf` | Recent files |

It also enables:

- **helm-mode** globally (Helm-powered `completing-read` everywhere)
- **helm-descbinds-mode** (Helm-powered `describe-bindings`)
- Helm for shell/eshell history
- Helm for etags
