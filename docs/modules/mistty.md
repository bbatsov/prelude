# Prelude MisTTY

[MisTTY](https://github.com/szermatt/mistty) is a shell/comint hybrid
built on top of `term.el`. It gives you full terminal emulation for TUI
programs while keeping ordinary Emacs editing and motion on the command
line. Unlike `vterm` it's pure Emacs Lisp, so there's no native module
to compile.

## Key bindings

Enabling this module rebinds Prelude's terminal key, <kbd>C-c t</kbd>,
from `crux-visit-term-buffer` to `mistty`, on the assumption that if
you've turned it on you'd rather that key opened MisTTY. You can pick a
different binding in your personal config if you prefer.

Inside a MisTTY buffer, <kbd>C-c C-j</kbd> and <kbd>C-c C-q</kbd> toggle
between the terminal and the Emacs editing modes.
