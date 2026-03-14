# Prelude TypeScript

!!! Note

    This module builds on top of the shared [Programming](programming.md) module.

## Packages

- [typescript-mode](https://github.com/emacs-typescript/typescript.el) -
  major mode for TypeScript
- [tide](https://github.com/ananthakumaran/tide) - TypeScript
  Interactive Development Environment

## Features

Tide provides a rich development experience for TypeScript:

- Code completion (via company-mode)
- Syntax checking (via Flycheck)
- Eldoc integration for inline type information
- Identifier highlighting (`tide-hl-identifier-mode`)
- Format on save (when `prelude-format-on-save` is enabled)

## Prerequisites

You need [Node.js](https://nodejs.org/) and `tsserver` (bundled with TypeScript)
installed. Install TypeScript globally:

```sh
npm install -g typescript
```

## Configuration

### Format on Save

By default, Tide formats the buffer before saving when `prelude-format-on-save`
is enabled. You can customize the format action:

```emacs-lisp
;; Disable format on save for TypeScript
(setq prelude-format-on-save nil)

;; Or use a different formatter
(setq prelude-ts-format-action #'my-custom-format-function)
```
