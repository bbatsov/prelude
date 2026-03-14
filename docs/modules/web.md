# Prelude Web

## Overview

This module provides support for editing web templates via
[web-mode](https://web-mode.org/), a major mode for editing HTML files with
embedded code (PHP, JSP, ERB, etc.).

## Supported File Types

`web-mode` is automatically activated for the following file extensions:

- `.html` and `.htm`
- `.phtml`, `.tpl`, `.tpl.php`
- `.jsp`, `.aspx`, `.ascx`
- `.erb`
- `.hbs` (Handlebars)
- `.blade.php` (Laravel Blade)
- PHP files under `views/`, `html/`, `theme/`, or `templates/` directories

## Smartparens Integration

The module integrates `web-mode` with `smartparens`, providing smart handling of
template tags:

- `%` pairs are matched automatically
- Shortcuts for ERB-style tags:
  - `%` + <kbd>SPC</kbd> expands to `<% | %>`
  - `%` + `=` expands to `<%= | %>`
  - `%` + `#` expands to `<%# | %>`

!!! Note

    `web-mode`'s built-in auto-pairing is disabled in favor of `smartparens`.
