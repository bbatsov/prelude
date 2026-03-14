# Prelude Perl

!!! Note

    This module builds on top of the shared [Programming](programming.md) module.

## cperl-mode

This module configures [cperl-mode](https://www.emacswiki.org/emacs/CPerlMode)
as the default Perl editing mode (replacing the built-in
`perl-mode`). `cperl-mode` provides better syntax
highlighting, indentation, and Perl-specific features.

Prelude configures the following defaults:

- **Indent level**: 4 spaces
- **Continued statement offset**: 8 spaces
- **Font lock** is enabled
- **Electric features** are mostly disabled (no
  auto-insertion of braces, parens, etc.) for a less
  intrusive experience
- **Lazy help** is enabled with a 3-second delay
- Clean face backgrounds for arrays and hashes

## Key Bindings

| Key | Command | Description |
|-----|---------|-------------|
| <kbd>C-h P</kbd> | `cperl-perldoc` | Look up Perl documentation |
