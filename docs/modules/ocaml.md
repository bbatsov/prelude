# Prelude OCaml

!!! Note

    This module builds on top of the shared [Programming](programming.md) module.

## Overview

Prelude provides powerful out-of-the-box experience for programming in OCaml:

- major-mode for editing OCaml code (`tuareg-mode`)
- integration with `utop`, OCaml's modern top-level (you can think of it as a REPL)
- linting via `flycheck` or `merlin`
- code completion via `merlin`

You can get similar experience using OCaml's LSP server, but it's based on Merlin internally and the setup with LSP is a bit more involved.

## Packages

When the `prelude-ocaml` is enabled it will install 3 packages:

- `tuareg-mode`
- `utop`
- `merlin`
- `flycheck-ocaml`

## Environment Setup

These setups for ocaml assume that you are using the OPAM package
manager (http://opam.ocaml.org/).

Because of the apparent complexity of getting Emacs environment
variables setup to use opam correctly, it is instead easier to use
opam itself to execute any necessary commands.

Also, the standard OCaml toplevel usage has been replaced in favor
of UTOP, the universal toplevel, and we assume that you are using
the Jane Street Core libraries rather than the regular OCaml
standard libraries

The minimum required setup for using Prelude's OCaml setup would be
to install OPAM, and then, minimally `opam install core utop'.  A
good getting started guide is available at
https://dev.realworldocaml.org/install.html

## Configuration

Prelude disables Merlin's own linting in favor of Flycheck. It also
leverages Merlin's company-mode backend instead of using directly
Merlin's rudimentary auto-completion system.
