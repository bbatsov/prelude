# Prelude LaTeX

## Package Prerequisites

You'll need a TeX distribution, e.g. [TeX Live](https://tug.org/texlive/)
(or [MacTeX](https://tug.org/mactex/) on macOS).

## LaTeX Mode

The module sets up [AUCTeX](https://www.gnu.org/software/auctex/), the
de facto standard environment for editing TeX and LaTeX in Emacs, with
the following defaults:

- AUCTeX parses your documents and saves what it learns about them, so
  it knows about your macros, labels, etc.
- AUCTeX asks for the master file of a document made of several files
  (`TeX-master` is `nil`).
- Typing `"` inserts a plain `"`, instead of the TeX quotes.
- `auto-fill-mode`, `abbrev-mode` and `subword-mode` are enabled.
- [Smartparens](https://github.com/Fuco1/smartparens) pairs delimiters
  the LaTeX way, unless you've turned smartparens off.

Completion of macros, environments, labels and citations comes from
AUCTeX itself (through `completion-at-point`), so it works with
[Company](company.md) and [Corfu](corfu.md) alike.

On macOS the `open` command is used to view DVI, PDF and HTML output.

## Fast Math Entry

Prelude supports two methods for fast math symbol entry, controlled by
the `prelude-latex-fast-math-entry` variable:

- `LaTeX-math-mode` (the default): AUCTeX's math mode, with
  <kbd>&#96;</kbd> as a prefix key (e.g. <kbd>&#96; a</kbd> inserts `\alpha`)
- `cdlatex`: [CDLaTeX](https://github.com/cdominik/cdlatex), for even
  faster input
- `nil`: no fast math entry

To switch to CDLaTeX, add to your personal config:

```emacs-lisp
(setq prelude-latex-fast-math-entry 'cdlatex)
```

## Key Bindings

A few of AUCTeX's commands:

<!-- markdownlint-disable MD013 -->

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>C-c C-c</kbd> | `TeX-command-master` | Compile the document (or run another command on it) |
| <kbd>C-c C-v</kbd> | `TeX-view` | View the output |
| <kbd>C-c C-e</kbd> | `LaTeX-environment` | Insert an environment |
| <kbd>C-c C-s</kbd> | `LaTeX-section` | Insert a section |
| <kbd>C-c ]</kbd> | `LaTeX-close-environment` | Close the current environment |
| <kbd>C-c C-f</kbd> | `TeX-font` | Change the font of the region (or insert it) |
| <kbd>C-c &#96;</kbd> | `TeX-next-error` | Go to the next error |

<!-- markdownlint-enable MD013 -->

Run <kbd>C-h m</kbd> in a LaTeX buffer for all key bindings.
