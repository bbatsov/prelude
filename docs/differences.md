# Differences from Stock Emacs

Prelude changes a lot of Emacs's defaults. Most of the changes are easy
to spot (a theme, line numbers, a welcome screen). This page is about
the ones that aren't: the things that work differently from what the
Emacs manual, a tutorial or your muscle memory will tell you. It's
also the place to look when something surprising happens and you wonder
whether Prelude is behind it.

Where there's an option to turn a behaviour off it's mentioned. The
options are described in [Configuration](configuration.md#prelude-options).

## Files get written without you saving them

- **Buffers are saved automatically** when you switch to another
  buffer or window, and when Emacs loses focus
  ([super-save](https://github.com/bbatsov/super-save)). There's no
  need for <kbd>C-x C-s</kbd> most of the time, but it also means that
  a half-finished edit ends up on disk. Option: `prelude-auto-save`.
- **Whitespace is cleaned up on save**: trailing whitespace and empty
  lines at the end of the file are removed, in the whole file. In
  someone else's project that can make for a noisy diff. Options:
  `prelude-clean-whitespace-on-save`, and
  `prelude-whitespace-cleanup-style` to touch only the lines you've
  edited.
- **Files always end with a newline** (`require-final-newline`).
- **Scripts become executable**: a file that starts with `#!` gets its
  executable bit set when it's saved.
- **Go and Rust files are formatted on save**, if you've enabled those
  modules. Option: `prelude-format-on-save`.
- **Buffers follow the files on disk**: when a file changes outside of
  Emacs, its buffer is reverted (`global-auto-revert-mode`), unless it
  has unsaved changes.
- **`.editorconfig` files are obeyed.**

## Where Emacs keeps its own files

- **Backup and auto-save files** (`foo.txt~`, `#foo.txt#`) go to the
  system's temporary directory instead of sitting next to your files.
  So does the undo history, which is kept between sessions. Mind that
  some systems empty that directory on reboot.
- **Session data** (recent files, the minibuffer history, places in
  files, bookmarks, Eshell's files) lives in `savefile/` in the Prelude
  directory.
- **Customize** writes to `personal/custom.el`, not to `init.el`.

## Editing

- **<kbd>C-w</kbd> with no active region kills the current line**, and
  <kbd>M-w</kbd> copies the word at point (more of it if you press
  <kbd>w</kbd>, <kbd>s</kbd> or <kbd>l</kbd> afterwards, see
  [easy-kill](https://github.com/leoliu/easy-kill)). Stock Emacs would
  use the text between point and the mark.
- **Typing replaces the selection** (`delete-selection-mode`).
- **<kbd>C-a</kbd> goes to the first non-blank character of the line**,
  and only then to the real beginning.
- **<kbd>TAB</kbd> indents, and completes if the line is indented
  already.**
- **Code that you yank is re-indented** in programming modes, unless
  it's long (`prelude-yank-indent-threshold`) or the language is
  sensitive to indentation, like Python
  (`prelude-indent-sensitive-modes`).
- **`indent-region` and `untabify` work on the whole buffer** when
  there's no active region.
- **<kbd>C-x C-x</kbd> doesn't activate the region**, it just swaps
  point and the mark.
- **Undo is a tree** ([undo-tree](https://www.dr-qubit.org/undo-tree.html)):
  <kbd>C-/</kbd> undoes, <kbd>C-?</kbd> redoes, and <kbd>C-x u</kbd>
  shows the tree of changes. In stock Emacs you'd redo by undoing the
  undo. Option: `prelude-undo-tree`.
- **Delimiters come in pairs**
  ([smartparens](https://github.com/Fuco1/smartparens)): type `(` and
  you get `()`. It also binds keys for moving over and reshaping
  expressions (e.g. <kbd>C-M-f</kbd>, <kbd>C-&lt;right&gt;</kbd>) that
  do other things in stock Emacs. Option: `prelude-smartparens`.
- **Indentation uses spaces**, never tabs (except where the language
  wants tabs, like Go and Makefiles).
- **Commands that Emacs disables for beginners are enabled**:
  narrowing, changing the case of the region, `erase-buffer` and
  <kbd>a</kbd> in Dired.
- **Some commands repeat with their last key** (`repeat-mode`): after
  <kbd>M-g n</kbd> another <kbd>n</kbd> goes to the next error. If
  the key you press next doesn't seem to insert itself, that's why.

## Keys that do something else

These standard keys run a different command in Prelude. The
[Usage](usage.md) page lists the keys that Prelude adds. The
completion modules replace a few more: with [Vertico](modules/vertico.md),
for instance, <kbd>C-x b</kbd>, <kbd>M-y</kbd> and <kbd>M-g g</kbd> run
the commands of Consult.

<!-- markdownlint-disable MD013 -->

| Key | Stock Emacs | Prelude |
| --- | ----------- | ------- |
| <kbd>C-x o</kbd> | `other-window` | `ace-window`: the same with two windows, with more you pick the window by its number |
| <kbd>C-x C-b</kbd> | `list-buffers` | `ibuffer` |
| <kbd>C-x m</kbd> | `compose-mail` | `eshell` |
| <kbd>C-x C-m</kbd> | prefix for the coding system commands (<kbd>C-x RET</kbd>) | `execute-extended-command`, i.e. <kbd>M-x</kbd> |
| <kbd>M-z</kbd> | `zap-to-char` | `zop-up-to-char`: stops before the character and shows what's about to go |
| <kbd>M-/</kbd> | `dabbrev-expand` | `hippie-expand` (option: `prelude-hippie-expand`) |
| <kbd>M-o</kbd> | unbound | `crux-smart-open-line` |
| <kbd>C-&lt;backspace&gt;</kbd> | `backward-kill-word` | `crux-kill-line-backwards` |
| <kbd>S-&lt;arrow&gt;</kbd> | extends the region | moves to the window in that direction (`windmove`) |
| <kbd>C-h C-f</kbd> | `view-emacs-FAQ` | `find-function` |
| right mouse button | extends the region | opens a context menu (`context-menu-mode`) |

<!-- markdownlint-enable MD013 -->

Many of Prelude's own keys live in `prelude-mode`, a minor mode that's
on everywhere. Its bindings take precedence over those of major modes,
which is why <kbd>C-a</kbd> is handed back to the terminal in
`ansi-term`, `vterm` and `eat`. <kbd>M-x prelude-mode</kbd> turns the
mode (and with it those keys) off and on again, everywhere.

Keys that use Super (<kbd>s-p</kbd>, <kbd>s-w</kbd> and so on) can be
turned off with `prelude-super-keybindings`.

### macOS and Windows

- On macOS **Command is Super and Option is Meta**
  (<kbd>C-c w</kbd> swaps them), and <kbd>Fn</kbd> is Hyper. The menu
  bar is always on, and Emacs gets its `PATH` from your shell, so that
  it finds the same programs as your terminal does.
- On Windows the Windows keys are Super and the Menu key is Hyper.

## Prompts and windows

- **Questions are answered with <kbd>y</kbd> or <kbd>n</kbd>**, never
  by typing out `yes` or `no`.
- **Help windows get selected** when they pop up, so <kbd>q</kbd>
  closes them right away.
- **Dired deletes and copies directories recursively** without asking
  about each level.
- **Compiling doesn't ask**: <kbd>M-x compile</kbd> saves the modified
  buffers and kills a compilation that's still running without a
  question.
- **Ediff stays in one frame.**
- **Splitting a window resizes all of them** proportionally, and
  <kbd>C-c &lt;left&gt;</kbd> restores the previous window layout
  (`winner-mode`).
- **A new buffer gets its major mode from its name**:
  <kbd>C-x b</kbd> `scratch.rb` gives you a Ruby buffer.
- **`emacsclient file:42`** opens the file at line 42.
- **A key that's waiting for more input shows what can follow** after
  a moment (`which-key`).
- **Missing major modes are installed on demand**: open a file of a
  kind that Prelude knows a package for (say, `.clj` or `Dockerfile`)
  and the package is installed.

## What you see

- No tool bar, no blinking cursor, no bell and no GNU startup screen
  (Prelude shows its own [welcome screen](configuration.md), option:
  `prelude-welcome-screen`).
- Line numbers in every buffer and a highlighted current line. Option:
  `prelude-minimalistic-ui` hides the line numbers and the menu bar.
- A [theme](configuration.md#color-themes) that changes between its
  light and its dark variant during the day.
- The names of most minor modes are hidden from the mode line.
- Changes relative to Git are marked in the fringe (`diff-hl`), and
  text that was just yanked or undone flashes briefly.
- The search shows the number of matches.
- A tip about Prelude in the echo area after startup.

## In programming modes

Most language modules build on
[prelude-programming](modules/programming.md), which turns on a few
things that stock Emacs leaves off:

- **Problems are highlighted as you type** by Flycheck (not Flymake,
  which Emacs and Eglot use by default), and the diagnostics of
  language servers are shown through Flycheck as well.
- **Whitespace problems are visible**: trailing whitespace, tabs and
  the part of a line beyond column 80 are highlighted. Option:
  `prelude-whitespace`.
- **Spelling is checked in comments and strings**, if `aspell` is
  installed. Option: `prelude-flyspell`.
- **`guru-mode` nags you** in the echo area when you use the arrow
  keys and friends instead of the Emacs equivalents. It only warns.
  Option: `prelude-guru`.
- **Tree-sitter modes replace the classic ones** (`python-ts-mode`
  instead of `python-mode`, and so on) when the grammar is there, and
  Prelude offers to install the grammar when it isn't. Hooks and
  settings made for the classic mode don't apply to the tree-sitter
  one. Option: `prelude-treesit-auto-install`.
- **A language server is started automatically** (with Eglot) when you
  open a file and the server is installed. Option: `prelude-lsp-client`.
- **Prelude's setup of a mode is a function on the mode's hook**, like
  `prelude-python-setup` on `python-mode-hook` and
  `python-ts-mode-hook`. It runs before the functions you add to the
  hook, and removing it from the hook leaves the mode alone. See
  [Configuration](configuration.md#personalization-example).
- <kbd>C-c p</kbd> is the prefix of
  [Projectile](https://docs.projectile.mx)'s commands. Option:
  `prelude-projectile`.

## Under the hood

- Packages come from MELPA in addition to GNU ELPA and NonGNU ELPA,
  i.e. you get the latest development snapshots, unless you pin
  packages to another archive (see `sample/prelude-pinned-packages.el`).
- The garbage collector runs less often (a threshold of 50 MB), and
  Emacs reads the output of subprocesses in bigger chunks, which
  helps language servers.
- Your own configuration is loaded from `personal/` after Prelude's
  (and from `personal/preload/` before it), not from `init.el`. See
  [Configuration](configuration.md#personalizing).
