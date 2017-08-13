# Prelude ERC Quickstart

## Customizing Server list

If you want to join a list of servers on `M-x start-irc`, other than
the default list, please redefine the variable `my-fav-irc` as follows
in your personal config

``` emacs-lisp 
(setq my-fav-irc '("irc.freenode.net" 
                   "irc.oftc.net"
                   "irc.mozilla.org" 
                   "irc.gnome.org")) 
```

## Customizing Last Quit Message

If you want to customize your IRC Last Quit Message from *Asta la
vista* to something more funkier, please redefine `bye-irc-message` as
follows

``` emacs-lisp 
(setq bye-erc-message "adios") 
```

## Reading NickServ passwords from auth-source plugin

If you want to automatically authenticate while logging into IRC
servers set the `erc-prompt-for-password` to nil as follows

``` emacs-lisp 
(setq erc-prompt-for-password nil) 
```

Now you can set password in plaintext in .authinfo file in the netRC
format or you it encrypted in .authinfo.gpg file after setting up gpg
in emacs

## Opening all ERC buffers in a new perspective
Many a time when we start IRC with the `start-irc` command, all the
channels open in our existing workspace, which can be annoying to
some; especially to those who like to organize their buffers into
separate groups (perspectives). To avoid this scenario, it is better
to group all the ERC buffers into one perspective called `IRC` when
`start-irc` is called. To enable this set the `prelude-new-irc-persp`
variable to true as follows

``` emacs-lisp 
(setq prelude-new-irc-persp t) 
```
