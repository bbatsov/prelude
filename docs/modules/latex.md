# Prelude LaTeX

## Overview

This module provides a comprehensive LaTeX editing setup powered by
[AUCTeX](https://www.gnu.org/software/auctex/), the premier TeX/LaTeX editing
package for Emacs.

## Packages

- [auctex](https://www.gnu.org/software/auctex/) - advanced
  TeX/LaTeX editing
- [cdlatex](https://github.com/cdominik/cdlatex) - fast
  insertion of math symbols and environments
- [company-auctex](https://github.com/alexeyr/company-auctex) -
  company-mode backend for AUCTeX (when company is available)
- [smartparens-latex](https://github.com/Fuco1/smartparens) -
  LaTeX-aware pair handling

## Configuration

Prelude sets the following AUCTeX defaults:

- **Auto-save** and **auto-parse** of TeX files are enabled
- **PDF mode** is the default output format
- **Multi-file support**: `TeX-master` is set to `nil`, so
  AUCTeX will ask for the master file
- **Auto-fill mode** is enabled for line wrapping
- **Abbrev mode** is enabled for common LaTeX abbreviations
- **Smartparens** is enabled for delimiter matching

### macOS

On macOS, Prelude configures the `open` command as the default viewer for DVI,
PDF, and HTML output.

## Fast Math Entry

Prelude supports two methods for fast math symbol entry, controlled by the
`prelude-latex-fast-math-entry` variable:

- **`LaTeX-math-mode`** (default) - AUCTeX's built-in math
  mode, activated with `` ` `` as a prefix key
- **`cdlatex`** - CDLaTeX mode for even faster input
- **`nil`** - disable fast math entry

To switch to CDLaTeX, add to your personal config:

```emacs-lisp
(setq prelude-latex-fast-math-entry 'cdlatex)
```
