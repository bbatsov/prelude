# Prelude Corfu

## Overview

[Corfu](https://github.com/minad/corfu) is a minimal in-buffer
completion UI for Emacs - a modern, lightweight alternative to
Company. It uses Emacs' built-in `completion-at-point` infrastructure
and the standard completion styles (so it cooperates with
`orderless` from `prelude-vertico`), instead of providing its own
backend abstraction the way Company does.

[Cape](https://github.com/minad/cape) extends
`completion-at-point-functions` with extra sources - file paths,
dabbrev, keywords, ispell, abbrevs, etc. - so they show up in the
Corfu popup alongside the language-specific completions provided by
LSP servers, Eglot, etc.

!!! Note

    This module is mutually exclusive with the
    [Company](company.md) module - enable one or the other, not
    both.

## Packages

- [corfu](https://github.com/minad/corfu) - the completion popup
- [cape](https://github.com/minad/cape) - completion-at-point extensions

## Defaults

Corfu auto-pops after typing 2 characters (with a 200 ms debounce),
<kbd>TAB</kbd> completes the selected candidate, and moving past the
last candidate takes you back to the first one:

| Variable | Value | Effect |
| -------- | ----- | ------ |
| `corfu-auto` | `t` | Open the popup automatically as you type |
| `corfu-auto-prefix` | `2` | Minimum prefix length to trigger |
| `corfu-auto-delay` | `0.2` | Delay (seconds) before popup |
| `corfu-cycle` | `t` | Wrap around at end of list |

`corfu-popupinfo-mode` is enabled by default - it shows
documentation for the highlighted candidate in a side popup.

## Default Cape sources

The following completion-at-point sources are added globally:

- `cape-dabbrev` - completes from words in open buffers
- `cape-file` - completes file paths

Add more from your personal config:

```emacs-lisp
(with-eval-after-load 'cape
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))
```

## Tips

- Press <kbd>M-SPC</kbd> in the popup to insert a space without
  dismissing it (useful with orderless).
- <kbd>M-g</kbd> opens the candidate at point (e.g. jumps to the
  function definition); <kbd>M-h</kbd> shows its documentation in a
  separate buffer.
- For language-specific completions to feel snappy, configure your
  LSP setup (Eglot or `lsp-mode`) to expose richer
  `completion-at-point-functions` - both already do this out of
  the box.
