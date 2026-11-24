# Prelude ERC

ERC is the IRC client that comes with Emacs. It's both powerful and
complex, so Prelude sets it up a bit and adds a couple of commands for
connecting to your servers and leaving them.

ERC is loaded only when you start using it, so enabling the module
doesn't slow down Emacs's startup.

## Connecting

<kbd>M-x prelude-erc-start</kbd> connects to every server in
`prelude-erc-servers`, over TLS (port 6697).
[Libera.Chat](https://libera.chat), where most of the free software
channels are, is the default. To connect to others, set the list in
your personal config:

```emacs-lisp
(setq prelude-erc-servers '("irc.libera.chat" "irc.oftc.net"))
```

ERC uses `erc-nick` as your nickname (or your user name, if that's not
set).

<kbd>M-x prelude-erc-stop</kbd> leaves all the servers with
`prelude-erc-quit-message` ("Asta la vista" by default).

## Settings

The module changes a few of ERC's defaults:

- The buffers of channels you leave, and of queries and the server when
  you quit it, are killed.
- Queries open in the current window.
- Joins, parts, quits and the like don't count as activity in the mode
  line.
- Long buffers get truncated (the `truncate` module).
- Messages are checked for spelling (the `spelling` module), unless
  `prelude-flyspell` is off.
- Everything is UTF-8.

If you turn on ERC's `log` module, the logs go to `~/.erc/logs`. If you
turn on `autoaway`, you're marked as away after 10 minutes without
using Emacs.

## NickServ passwords

To log in to NickServ without typing your password, keep it in
`~/.authinfo.gpg` (or `~/.authinfo`) and tell ERC not to ask for it:

```emacs-lisp
(setq erc-prompt-for-password nil)
```

## A perspective for IRC

If you use [perspective](https://github.com/nex3/perspective-el),
setting `prelude-new-irc-persp` keeps all the ERC buffers in a
perspective called `IRC`:

```emacs-lisp
(setq prelude-new-irc-persp t)
```

## Old names

`start-irc`, `stop-irc`, `my-fav-irc` and `bye-irc-message` are the old
names of `prelude-erc-start`, `prelude-erc-stop`, `prelude-erc-servers`
and `prelude-erc-quit-message`. They still work, but they're obsolete.
