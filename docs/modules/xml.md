# Prelude XML

!!! Note

    This module is enabled by default.

## XML Mode

The module uses the `nxml-mode` that comes with Emacs for XML files,
including files that start with `<?xml` and Maven's `.pom` files.

Prelude sets the following defaults:

- Child elements and attributes are indented by 4 spaces.
- Typing `</` finishes the closing tag for you.
- Completion (`completion-at-point`) knows the names of elements and
  attributes.
- No XML declaration is inserted in new files.
- `subword-mode` is enabled, so the editing commands understand
  `CamelCase`.

## Key Bindings

A few of the commands of `nxml-mode`:

<!-- markdownlint-disable MD013 -->

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>C-c C-f</kbd> | `nxml-finish-element` | Insert the end tag of the element at point |
| <kbd>C-c C-b</kbd> | `nxml-balanced-close-start-tag-block` | Close the start tag and add the end tag on a line of its own |
| <kbd>C-c RET</kbd> | `nxml-split-element` | Split the element at point in two |

<!-- markdownlint-enable MD013 -->

Run <kbd>C-h m</kbd> in an XML buffer for all key bindings.
