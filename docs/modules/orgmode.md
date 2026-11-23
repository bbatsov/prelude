# Prelude Org

!!! Note

    This module builds on the built-in [Org mode](https://orgmode.org).

## Org Mode

The module adds the global key bindings that the Org manual suggests:

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>C-c l</kbd> | `org-store-link` | Store a link to the current location |
| <kbd>C-c a</kbd> | `org-agenda` | Open the agenda dispatcher |
| <kbd>C-c c</kbd> | `org-capture` | Capture a note or a task |

In Org buffers `org-indent-mode` is enabled, and <kbd>C-a</kbd> does
what Org wants it to do, instead of what Prelude binds it to everywhere
else.

## org-habits

It enables [org-habits][habits] and
[tracks TODO state changes][todo-changes] into a
[drawer][drawers].

[habits]: https://orgmode.org/manual/Tracking-your-habits.html "org-habits"
[todo-changes]: https://orgmode.org/manual/Tracking-TODO-state-changes.html "todo-state-changes"
[drawers]: https://orgmode.org/manual/Drawers.html "org-drawers"
