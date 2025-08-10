# Prelude OrgMode

!!! Note
    This module builds on the Emacs OrgMode Functionality

## OrgMode

The module establishes some sensible defaults for `orgmode`

It establishes a few extra keybidings:

- `C-c l` (`org-store-link`)
- `C-c a` (`org-agenda`)
- `C-c b` (`org-switchb`)

### Shift-arrow keybinding conflicts

Windmove arrow keybindings are the default for Prelude, but org-mode has some
specific bindings for the S-[arrow keys].

If a user adds the following code, org-mode buffers will have standard org-mode
bindings, but other buffers will use windmove bindings.

```lisp
(prelude-enable-org-mode-shift-bindings)
```

## org-habits

It enables [org-habits](https://orgmode.org/manual/Tracking-your-habits.html "org-habits") and [tracks TODO state changes](https://orgmode.org/manual/Tracking-TODO-state-changes.html "todo-state-changes") into a
[drawer](https://orgmode.org/manual/Drawers.html "org-drawers").
