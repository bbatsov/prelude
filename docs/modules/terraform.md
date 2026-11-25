# Prelude Terraform

!!! Note

    This module builds on top of the shared [Programming](programming.md)
    module and the [LSP](lsp.md) module.

## Package Prerequisites

You'll need `terraform` and its language server,
[terraform-ls](https://github.com/hashicorp/terraform-ls).

## Terraform Mode

The module uses
[terraform-mode](https://github.com/hcl-emacs/terraform-mode) for
editing Terraform files. A few of its commands:

<!-- markdownlint-disable MD013 -->

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>C-c C-d C-w</kbd> | `terraform-open-doc` | Open the documentation of the resource at point |
| <kbd>C-c C-d C-c</kbd> | `terraform-kill-doc-url` | Copy the URL of that documentation |
| <kbd>C-c C-d C-r</kbd> | `terraform-insert-doc-in-comment` | Insert that URL as a comment |
| <kbd>C-c C-f</kbd> | `outline-toggle-children` | Fold or unfold the block at point |

<!-- markdownlint-enable MD013 -->

<kbd>M-x terraform-format-buffer</kbd> runs `terraform fmt`. To do that every
time you save a file:

```emacs-lisp
(setq terraform-format-on-save t)
```

## Key Bindings

See the [LSP](lsp.md) module for the available LSP key bindings.
