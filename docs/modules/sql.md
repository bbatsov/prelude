# Prelude SQL

!!! Note

    This module builds on top of the shared [Programming](programming.md) module.

## SQL Mode

Emacs comes with `sql-mode`, which can also run the console of pretty
much any database (`M-x sql-postgres`, `M-x sql-mysql`, `M-x sql-sqlite`,
etc). When a console is running you can send code to it straight from
a SQL buffer:

<!-- markdownlint-disable MD013 -->

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>C-c C-c</kbd> | `sql-send-paragraph` | Send the current statement |
| <kbd>C-c C-r</kbd> | `sql-send-region` | Send the region |
| <kbd>C-c C-b</kbd> | `sql-send-buffer` | Send the buffer |
| <kbd>C-c C-z</kbd> | `sql-show-sqli-buffer` | Show the console |

<!-- markdownlint-enable MD013 -->

`sql-mode` highlights standard SQL by default. Use
`M-x sql-set-product` (or set `sql-product`) to get the keywords of
your database.

The module adds a couple of things on top:

- [sql-indent](https://github.com/alex-hhh/emacs-sql-indent) indents
  code according to the syntax of the statement, instead of simply
  lining it up with the previous line.
- Long lines aren't wrapped in the console, so wide result sets stay
  readable.

## LSP support

A language server is started only when
[sqls](https://github.com/sqls-server/sqls) is installed:

```sh
go install github.com/sqls-server/sqls@latest
```

It needs to know how to connect to your database to be of much use, so
take a look at its documentation. See the [LSP](lsp.md) module for the
available LSP key bindings.
