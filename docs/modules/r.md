# Prelude R

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## Package Prerequisites

You'll need R and its language server, which is an R package:

```r
install.packages("languageserver")
```

## ESS

The module uses [ESS](https://ess.r-project.org/) (Emacs Speaks
Statistics), which gives you a major mode for R code (`ess-r-mode`), an
R console inside Emacs and a lot more. Some of the essential key
bindings:

<!-- markdownlint-disable MD013 -->

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>C-c C-z</kbd> | `ess-switch-to-inferior-or-script-buffer` | Switch between the code and the R console |
| <kbd>C-RET</kbd> | `ess-eval-region-or-line-visibly-and-step` | Evaluate the region or the current line |
| <kbd>C-c C-c</kbd> | `ess-eval-region-or-function-or-paragraph-and-step` | Evaluate the region, function or paragraph |
| <kbd>C-c C-b</kbd> | `ess-eval-buffer` | Evaluate the buffer |
| <kbd>C-c C-v</kbd> | `ess-display-help-on-object` | Show the documentation of an object |

<!-- markdownlint-enable MD013 -->

<kbd>M-x R</kbd> starts an R console. Check out the
[ESS manual](https://ess.r-project.org/Manual/ess.html) for everything
else.

## Key Bindings

See the [LSP](lsp.md) module for the available LSP key bindings.
