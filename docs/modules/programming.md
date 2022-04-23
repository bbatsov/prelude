# Prelude Programming

Prelude's programming module enables some additional functionality
for `prog-mode` - the parent mode for all major programming modes in Emacs.

Here are some features it provides:

* spell-checking of comments (via `flyspell-prog-mode`)
* auto-pairing of delimiters like parentheses (via `smartparens`)
* visual ques for whitespace (via `whitespace-mode`)
* highlighting code annotations (via `hl-todo`)
* linter integration (via `flycheck`)
* showing current definition name in the modeline (via `which-func`)

Most of this boils down to enabling a bunch of minor modes in `prog-mode-hook`.
