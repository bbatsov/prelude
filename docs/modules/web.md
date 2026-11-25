# Prelude Web

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module.

    This module is enabled by default.

## Web Mode

The module uses [web-mode](https://web-mode.org/), a major mode for
editing HTML, and HTML templates with embedded code (PHP, JSP, ERB,
etc.), for the following files:

- `.html` and `.htm`
- `.phtml`, `.tpl`, `.tpl.php`
- `.jsp`, `.aspx`, `.ascx`
- `.erb`
- `.hbs` (Handlebars), `.mustache`
- `.njk`, `.jinja`, `.jinja2`
- `.svelte`, `.vue`
- `.blade.php` (Laravel Blade)
- PHP files under `views/`, `html/`, `theme/`, or `templates/` directories

Markup, CSS and code are indented by 2 spaces, and `subword-mode` is
enabled, so the editing commands understand `CamelCase`.

## Smartparens Integration

When smartparens is on, it takes care of pairing delimiters in
`web-mode` buffers instead of `web-mode` itself, and it knows about
ERB-style template tags:

- `%` pairs are matched automatically
- `%` + <kbd>SPC</kbd> expands to `<% | %>`
- `%` + `=` expands to `<%= | %>`
- `%` + `#` expands to `<%# | %>`

## Key Bindings

A few of the commands of `web-mode`:

<!-- markdownlint-disable MD013 -->

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>C-c C-f</kbd> | `web-mode-fold-or-unfold` | Fold or unfold the element at point |
| <kbd>C-c C-n</kbd> | `web-mode-navigate` | Jump to the matching tag |
| <kbd>C-c C-e /</kbd> | `web-mode-element-close` | Close the element at point |
| <kbd>M-;</kbd> | `web-mode-comment-or-uncomment` | Comment or uncomment |
| <kbd>C-c TAB</kbd> | `web-mode-buffer-indent` | Indent the buffer |

<!-- markdownlint-enable MD013 -->

Run <kbd>C-h m</kbd> in a `web-mode` buffer for all key bindings.
