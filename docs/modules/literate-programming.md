# Prelude Literate Programming

!!! Note

    This module builds on top of [Org mode](orgmode.md), which comes
    with Emacs.

The module sets up Org mode for
[literate programming](https://www-cs-faculty.stanford.edu/~knuth/lp.html):
code blocks in Org files that you can run, with their results right
below them, and Jupyter notebooks.

## Code blocks

Org Babel runs the code blocks of these languages:

- Python, also through Jupyter's IPython kernel (`ipython`), with
  [ob-ipython](https://github.com/gregsexton/ob-ipython)
- shell
- JavaScript, with Node.js (`js`) or with Deno (`deno`, through
  [ob-deno](https://github.com/isamert/ob-deno))
- TypeScript, with
  [ob-typescript](https://github.com/lurdan/ob-typescript)

To change the list, set `prelude-ob-loader-list` in
`personal/preload`:

```emacs-lisp
(setq prelude-ob-loader-list '((python . t) (shell . t) (sql . t)))
```

Org asks before it runs a code block, which keeps an Org file that you
got from somewhere else from running its code when you export it, for
instance. If you only ever run your own code, you can turn the question
off:

```emacs-lisp
(setq org-confirm-babel-evaluate nil)
```

[ob-async](https://github.com/astahlman/ob-async) runs code blocks in
the background when they have an `:async` header argument, and
[ob-tmux](https://github.com/ahendriksen/ob-tmux) runs shell code
blocks in tmux.

## Jupyter notebooks

[EIN](https://github.com/millejoh/emacs-ipython-notebook) is a client
for Jupyter: <kbd>M-x ein:run</kbd> starts a Jupyter server and lists
its notebooks. [code-cells](https://github.com/astoff/code-cells.el)
opens notebooks (`.ipynb` files) as plain scripts that you can edit and
save, which takes [Jupytext](https://jupytext.readthedocs.io).

## Package Prerequisites

The code blocks of a language run only if its tools are installed:
Jupyter (which EIN needs as well) for `ipython`, Node.js for `js`, Deno
for `deno` and the TypeScript compiler (`tsc`) for `typescript`.
