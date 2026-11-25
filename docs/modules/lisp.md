# Prelude Lisp

!!! Note

    This module is enabled by default.

Basic shared configuration for Lisp-like programming languages.
This module provides:

- `smartparens-strict-mode` in Lisp major modes and REPL buffers
  (prevents unbalanced delimiter deletion)
- `rainbow-delimiters` in Lisp major modes and REPL buffers

The setups of the Lisp language modules get those from
`prelude-lisp-coding-setup` (and from
`prelude-interactive-lisp-coding-setup` in REPL buffers). To give
another Lisp mode the same, add the function to the hook of the mode:

```emacs-lisp
(add-hook 'fennel-mode-hook #'prelude-lisp-coding-setup)
```

Prelude uses smartparens with its own default keybinding set, not
the paredit compatibility set. This avoids keybinding conflicts
with standard Emacs prefixes like <kbd>M-s</kbd> (`search-map`) and <kbd>M-?</kbd>
(`xref-find-references`). See the
[Programming module docs](programming.md#smartparens-keybindings)
for details and workarounds if you use paredit.
