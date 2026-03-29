# Prelude Lisp

Basic shared configuration for Lisp-like programming languages.
This module provides:

- `smartparens-strict-mode` in Lisp major modes and REPL buffers
  (prevents unbalanced delimiter deletion)
- `rainbow-delimiters` in Lisp major modes and REPL buffers

Prelude uses smartparens with its own default keybinding set, not
the paredit compatibility set. This avoids keybinding conflicts
with standard Emacs prefixes like `M-s` (`search-map`) and `M-?`
(`xref-find-references`). See the
[Programming module docs](programming.md#smartparens-keybindings)
for details and workarounds if you use paredit.
