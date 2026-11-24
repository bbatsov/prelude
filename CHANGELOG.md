<!-- markdownlint-disable MD013 MD024 -->

# Change log

## Unreleased

### New features

- Add a welcome screen shown on startup (unless Emacs is launched to open specific files). It points you at the essentials (personal config location, how to enable modules, the user manual) and offers quick access to recent files, known projects and bookmarks, plus a rotating tip. Reopen it any time with `M-x prelude-welcome`, or turn it off with `(setq prelude-welcome-screen nil)`.
- [#1466](https://github.com/bbatsov/prelude/issues/1466): Add `prelude-java` module: Java support via `java-ts-mode` and the Eclipse JDT language server, which is installed automatically (by [eglot-java](https://github.com/yveszoundi/eglot-java) or [lsp-java](https://emacs-lsp.github.io/lsp-java/), depending on `prelude-lsp-client`), with key bindings for running tests and main classes.
- [#1469](https://github.com/bbatsov/prelude/pull/1469): Add `prelude-csharp` module: C# support via the built-in `csharp-ts-mode` (or `csharp-mode`) and LSP.
- [#1469](https://github.com/bbatsov/prelude/pull/1469): Add `prelude-julia` module: Julia support via `julia-mode`, [julia-repl](https://github.com/tpapp/julia-repl) and LSP (LanguageServer.jl gets installed by `eglot-jl` or `lsp-julia`).
- [#1469](https://github.com/bbatsov/prelude/pull/1469): Add `prelude-kotlin` module: Kotlin support via [kotlin-mode](https://github.com/Emacs-Kotlin-Mode-Maintainers/kotlin-mode) and LSP.
- [#1469](https://github.com/bbatsov/prelude/pull/1469): Add `prelude-nix` module: Nix support via [nix-mode](https://github.com/NixOS/nix-mode) and LSP.
- [#1469](https://github.com/bbatsov/prelude/pull/1469): Add `prelude-php` module: PHP support via [php-mode](https://github.com/emacs-php/php-mode) and LSP.
- [#1469](https://github.com/bbatsov/prelude/pull/1469): Add `prelude-r` module: R support via [ESS](https://ess.r-project.org/) and LSP.
- [#1469](https://github.com/bbatsov/prelude/pull/1469): Add `prelude-sql` module: syntax-aware indentation ([sql-indent](https://github.com/alex-hhh/emacs-sql-indent)) for the built-in `sql-mode`, unwrapped result sets in SQL consoles, and LSP when [sqls](https://github.com/sqls-server/sqls) is installed.
- [#1469](https://github.com/bbatsov/prelude/pull/1469): Add `prelude-terraform` module: Terraform support via [terraform-mode](https://github.com/hcl-emacs/terraform-mode) and LSP.
- [#1469](https://github.com/bbatsov/prelude/pull/1469): Add `prelude-zig` module: Zig support via [zig-mode](https://github.com/ziglang/zig-mode) and LSP.
- Add `prelude-swift` module: Swift support via the tree-sitter powered `swift-ts-mode` and `sourcekit-lsp` (registered with Eglot, since Eglot has no built-in entry for Swift). Starts an LSP session through the usual `prelude-lsp-enable` abstraction.
- Add `prelude-mistty` module: a terminal based on [MisTTY](https://github.com/szermatt/mistty), a pure-elisp shell/comint hybrid on top of `term.el` (no native module to compile). Rebinds Prelude's terminal key `C-c t` to `mistty`.
- Add a `prelude-spell-checker` option to choose between `flyspell` (the default) and [jinx](https://github.com/minad/jinx), a faster libenchant-based spell checker that only checks the visible part of the buffer. Jinx is installed on demand and enabled as a single global mode; set `(setq prelude-spell-checker 'jinx)` in your personal config to opt in (requires libenchant and a C compiler).
- Add a `prelude-whitespace-cleanup-style` option to choose how whitespace is cleaned on save: `whitespace-cleanup` (the default, tidies the whole file) or [ws-butler](https://github.com/lewang/ws-butler), which trims only the lines you actually edited so saving a file in someone else's project no longer produces noisy whitespace-only diffs. ws-butler is installed on demand.
- Add `prelude-ai` module: a thin wrapper around [gptel](https://github.com/karthink/gptel) for LLM-backed chat (Claude, GPT, Gemini, Ollama, etc.). Binds `gptel-menu` to `C-c q`. Backends and API keys are configured in personal config -- see the module documentation for examples.
- Add `prelude-forge` module: enables [Forge](https://github.com/magit/forge) on top of Magit so you can read and reply to GitHub/GitLab/Gitea pull requests and issues without leaving Emacs.
- Add `prelude-eglot-booster` module: speeds up Eglot via the [emacs-lsp-booster](https://github.com/blahgeek/emacs-lsp-booster) wrapper. The Emacs side ([eglot-booster](https://github.com/jdtsmith/eglot-booster)) is auto-installed via `package-vc-install` when the booster binary is on `PATH`; otherwise the module no-ops.
- Add `prelude-corfu` module: a modern, lightweight in-buffer completion stack ([corfu](https://github.com/minad/corfu) + [cape](https://github.com/minad/cape)) -- alternative to `prelude-company`. Pairs naturally with the vertico/orderless setup in `prelude-vertico`.
- Add `prelude-apheleia` module: enables [Apheleia](https://github.com/radian-software/apheleia) globally for async, flicker-free format-on-save (Prettier, Black, Ruff, gofmt, rustfmt, ...). Supersedes the per-language format hooks in modules like `prelude-rust` and `prelude-go`.
- [#1460](https://github.com/bbatsov/prelude/issues/1460): Load `personal/early-init.el` from Prelude's `early-init.el`, so you can keep your own early startup settings without editing Prelude's files.
- Warn at startup when modules that do the same job are enabled together (e.g. `prelude-vertico` and `prelude-ivy`, or `prelude-company` and `prelude-corfu`).
- [#1461](https://github.com/bbatsov/prelude/pull/1461): Add `prelude-smartparens` user option, allowing smartparens support to be disabled.
- Offer to install a missing tree-sitter grammar the first time a file in that language is opened, and fall back to the classic major mode if it isn't installed. This works the same way on Emacs 29 through 31 and for tree-sitter modes from packages. Control it with `prelude-treesit-auto-install` (`ask`, `always` or `nil`), and install the grammars of all enabled modules with `M-x prelude-treesit-install-grammars`. Prelude's own grammar recipes are pinned to releases close to the ones Emacs 31 picked, and on Emacs 31 Emacs's recipes are used instead.

### Changes

- [#1489](https://github.com/bbatsov/prelude/pull/1489): Add a manual page on how Prelude's behaviour differs from stock Emacs in ways that aren't obvious.
- [#1489](https://github.com/bbatsov/prelude/pull/1489): Enable `repeat-mode` (e.g. `M-g n n n` keeps going to the next error) and `context-menu-mode` (a right click brings up a context menu instead of extending the region).
- [#1488](https://github.com/bbatsov/prelude/pull/1488): Show Eglot's diagnostics with Flycheck's own `flycheck-eglot-mode` (a part of Flycheck since version 39) instead of the `flycheck-eglot` package, which is no longer installed.
- [#1487](https://github.com/bbatsov/prelude/pull/1487): Stop the Go, Ivy and Org modules from taking keys that belong to others. Go's test commands are now on `C-c C-t t`, `C-c C-t f` and `C-c C-t p` (the keys of `go-ts-mode` in Emacs 31) and `go-run` is on `C-c C-r`; Ivy no longer binds `C-c C-r`, `C-c g`, `C-c j`, `C-c a` and `C-x l`; Org no longer binds `C-c b`.
- [#1486](https://github.com/bbatsov/prelude/pull/1486): Make [Tokyo Night](https://github.com/bbatsov/tokyo-night-emacs) the default theme, with its light variant during the day and the dark one at night: `prelude-theme` can now be a `(DAY . NIGHT)` pair, and `prelude-theme-daytime` says when to switch (with the system's light and dark mode where Emacs knows about it, by the clock elsewhere). Put `(setq prelude-theme 'zenburn)` in `personal/preload` to keep Zenburn.
- [#1483](https://github.com/bbatsov/prelude/pull/1483): Make all of Prelude's options work when they're set with `M-x customize`. Options that are read while Prelude starts (the theme, the minimalistic UI, the LSP client, etc.) used to be ignored there, as `custom.el` is loaded after the core; Prelude now reads its own options from it first.
- [#1483](https://github.com/bbatsov/prelude/pull/1483): Make `prelude-auto-save` and `prelude-user-init-file` do something again. The first turns `super-save` off when set to `nil`, the second is what `C-c I` (now `prelude-find-user-init-file`) opens: your personal folder rather than Prelude's `init.el`.
- [#1479](https://github.com/bbatsov/prelude/pull/1479): Stop forcing Apple Color Emoji on the whole Unicode range on macOS. That was a workaround from 2015; the Emacs versions Prelude supports pick a color font for emoji by themselves. Prelude doesn't configure any fonts now, and the manual shows how to set your own.
- [#1472](https://github.com/bbatsov/prelude/pull/1472): Start a language server for shell scripts, CSS, YAML and JSON when there's one installed (`prelude-shell`, `prelude-css`, `prelude-yaml` and `prelude-js`). A missing server isn't mentioned for those, and for SQL, as `prelude-lsp-enable` now takes a `quiet` argument.
- [#1471](https://github.com/bbatsov/prelude/pull/1471): Don't start Eglot in buffers whose language server isn't installed. Prelude mentions the missing server once for a major mode, instead of Eglot warning about it for every file you open.
- [#1470](https://github.com/bbatsov/prelude/pull/1470): `prelude-format-on-save` now controls the format on save of the Go and Rust modules, which also leave the formatting to Apheleia when `prelude-apheleia` is enabled (it now prefers `goimports` to `gofmt` as well).
- Add an `early-init.el` that tunes startup: it raises the GC threshold while Emacs loads (restoring a modest value once startup is over), disables the tool bar via frame parameters so the initial frame is never drawn with one, sets `frame-inhibit-implied-resize`, native-compiles packages at install time (`package-native-compile`), and sets a sane `LANG` for GUI Emacs on macOS (which otherwise starts in the `C` locale and breaks spell-checker dictionaries and subprocess sorting).
- Modernize the Eglot event-log setting to prefer `eglot-events-buffer-config` on newer Eglot, falling back to the obsolete `eglot-events-buffer-size` on older versions.
- Bridge Eglot diagnostics into Flycheck via [flycheck-eglot](https://github.com/flycheck/flycheck-eglot) when `prelude-lsp-client` is `eglot`, so LSP diagnostics show up in Prelude's Flycheck UI instead of only through Flymake. Installed on demand; lsp-mode users are unaffected (lsp-mode has its own Flycheck integration).
- Add [Embark](https://github.com/oantolin/embark) (and `embark-consult`) to `prelude-vertico`: a keyboard-driven context menu bound to `C-.` (`embark-act`), `C-;` (`embark-dwim`) and `C-h B` (`embark-bindings`), with `embark-export` into editable grep/occur buffers. In Flyspell buffers those keys keep their Flyspell auto-correct meaning; Embark still works in the minibuffer and elsewhere.
- Enable the `vertico-directory` extension in `prelude-vertico`: `RET` descends into the selected directory, `DEL` deletes a whole path component, and `M-DEL` deletes a word of it.
- Enable `corfu-history-mode` in `prelude-corfu` so recently chosen completion candidates sort first (persisted across sessions via savehist).
- Set `consult-narrow-key` to `<` in `prelude-vertico`, so you can narrow consult candidates to a single group (e.g. `< b` for buffers in `consult-buffer`).
- Tidy up `prelude-common-lisp`: drop stale `slime-autodoc-use-multiline-p` setting (the variable was removed from upstream SLIME; modern autodoc honors `eldoc-echo-area-use-multiline-p`), set `inferior-lisp-program` to `sbcl` so `M-x run-lisp` works without SLIME, and add `slime-quicklisp` to `slime-contribs` for Quicklisp integration.
- Enable `lexical-binding` in all of Prelude's Emacs Lisp files (including the sample `prelude-modules.el`), which silences the missing-cookie warnings Emacs 31 prints on every startup.
- Modules now `add-hook` their defaults to Prelude's hooks (`prelude-<lang>-mode-hook`, `prelude-lisp-coding-hook`, etc.) instead of `setq`-ing them, so functions you add to those hooks in `personal/preload` are no longer thrown away when the module loads.
- Stop installing `gist` (unused), and only install `which-key` and `editorconfig` on Emacs 29, since both are built into Emacs 30+.
- Replace the unmaintained `smartrep` package with a plain transient keymap for the `C-c .` operate-on-number bindings. They work the same way: after the first operation, the operator keys can be repeated without the prefix.
- Install all of Prelude's packages with `use-package` and `:ensure t`, and deprecate `prelude-require-package` and `prelude-require-packages` in favour of it. `prelude-packages` now starts out empty and gets filled in with every package Prelude ensures (including ones from modules), so `prelude-update-packages` and `prelude-list-foreign-packages` keep working, and your own `use-package :ensure` forms are tracked the same way.
- Start up a lot faster (about 1.3s down to 0.4s in a terminal with the default modules): the shell environment is only fetched for GUI frames and daemons, packages are no longer initialized twice, and TRAMP, Eglot, calc, crux and the Perl, shell and XML modes are only loaded when they're actually used.

### Bugs fixed

- [#1491](https://github.com/bbatsov/prelude/pull/1491): Match only whole file names for prezto's files (any file ending in `zshrc` opened in `sh-mode`).
- [#1491](https://github.com/bbatsov/prelude/pull/1491): Stop the LaTeX module from installing `company-auctex` (unmaintained, and pulling in Company for Corfu users); AUCTeX's own completion works with both.
- [#1491](https://github.com/bbatsov/prelude/pull/1491): Set up `rust-mode` buffers (the fallback for when there's no Rust grammar) like `rust-ts-mode` ones, with LSP, formatting on save and Cargo.
- [#1490](https://github.com/bbatsov/prelude/pull/1490): Indent C and C++ in the K&R style with 4 spaces as documented, in the tree-sitter modes (which used 2 spaces) and from the first buffer on, without overriding styles set by the user, and stop running the C setup in Java and the other `cc-mode` buffers that aren't C, C++ or Objective-C, where it reported a missing language server that `eglot-java` was about to install.
- Update `prelude-helm` to current upstream variable/function names: rename `helm-google-suggest-use-curl-p` to `helm-net-prefer-curl` (deprecated alias since helm 1.7.7), rename `helm-split-window-in-side-p` to `helm-split-window-inside-p` (deprecated alias since helm 2.8.6), drop `helm-ff-file-name-history-use-recentf` (removed from helm; behavior is now unconditional), and drop the `C-c w` binding to `helm-wikipedia-suggest` (function moved to a separate `helm-wikipedia` package).
- Drop `C-c C-l` binding to `helm-comint-input-ring` in `prelude-helm-everywhere` -- that command was removed from upstream helm.
- Replace stale `erc-autoaway-use-emacs-idle` setting in `prelude-erc` with `(setq erc-autoaway-idle-method 'emacs)` (the old variable was removed; idle source is now selected via `erc-autoaway-idle-method`).
- Rebind `M-s m` from removed `consult-multi-occur` to built-in `multi-occur` in `prelude-vertico` (consult dropped the wrapper in favor of the built-in command).
- [#1450](https://github.com/bbatsov/prelude/issues/1450): Remove broken `slime-complete-symbol-function` setting in `prelude-common-lisp` (referenced an undefined `slime-flex-completions` symbol; the variable itself has been obsolete since 2015).
- [#1453](https://github.com/bbatsov/prelude/issues/1453): Fix Common Lisp completion in `prelude-common-lisp`: call `slime-setup` so the configured contribs (and `slime-c-p-c-completion-at-point`) actually load, and skip SLIME completion when no Swank session is connected so company no longer errors with a void-function or "Not connected." message.
- Fix `prelude-ocaml` integration with `neocaml`: hook into `neocaml-base-mode` (so `.mli` files are covered), enable `ocaml-eglot-mode` properly, and call `prelude-lsp-enable` so the LSP server actually starts.
- Use `ocaml-eglot-mode` instead of the obsolete `ocaml-eglot` alias (renamed in `ocaml-eglot` 1.4.0).
- Add a temporary `eglot-server-programs` entry for `neocaml` so older `neocaml` versions still get `ocamllsp` started. Can be removed once `neocaml >= 20260331` is widely available on MELPA.
- Drop `M-g e` and `M-g f` from `prelude-vertico`'s consult bindings so they no longer shadow the avy bindings (`avy-goto-word-0`, `avy-goto-line`) set in core. Bind `consult-compile-error` / `consult-flymake` in your personal config if you want them.
- [#1454](https://github.com/bbatsov/prelude/issues/1454): Drop stale `tide` references from the docs now that `prelude-ts` uses `typescript-ts-mode` + LSP.
- [#1462](https://github.com/bbatsov/prelude/issues/1462): Don't break the `prelude-ocaml` mode hook (and with it font-locking and line numbers) when `ocaml-eglot` isn't installed.
- [#1462](https://github.com/bbatsov/prelude/issues/1462): Refresh the package archives and retry once when installing a package fails, so a stale package cache (listing versions MELPA no longer has) no longer leaves packages uninstalled. Covers `use-package` `:ensure` as well as `prelude-require-package`.
- Replace obsolete APIs flagged by the byte-compiler: `company-show-numbers` (now `company-show-quick-access`), `helm-projectile-on` (now `helm-projectile-mode`), `racket-unicode-input-method-enable` (now `racket-input-mode`), `erc-server-buffer-p` on ERC 5.6+, and the `racket-repl-visit-definition` binding on `M-.` (racket-mode uses xref now, so the global `M-.` already does the right thing). Drop the redundant `uniquify-after-kill-buffer-p` setting, which is on by default and obsolete in Emacs 31.
- Fix package installs failing for the rest of the session after installing a package whose own code runs `use-package` with `:ensure` at compile time (such as recent SLIME with its `slime-xterm-color` contrib). The package name was recorded with its source position, which broke every later install on Emacs 30 and 31.
- Don't break `org-mode` in `prelude-literate-programming` when Jupyter isn't installed: `ob-ipython` queried Jupyter for its kernels every time an Org buffer was opened and signaled an error if it wasn't there.
- Make `prelude-update` stop with an error (showing the `git` output) when the checkout can't be fast-forwarded, instead of reporting success, and stop it from changing the current buffer's directory.
- Don't install LSP-client-specific packages (`lsp-dart`, `ocaml-eglot`, `eglot-fsharp`) for users of the other LSP client.
- Keep installing the packages of `use-package :ensure` forms after Prelude gets byte-compiled (as `prelude-update` does). `use-package` ensures packages at compile time when a file is compiled, so the compiled files never installed missing packages (and installed conditional ones regardless of their condition).
- [#1470](https://github.com/bbatsov/prelude/pull/1470): Fix opening a Rust file turning off "make scripts executable on save" for every buffer (the function on `after-save-hook` is now `prelude-make-script-executable`, which knows that `#![...]` isn't a shebang).
- [#1470](https://github.com/bbatsov/prelude/pull/1470): Format Rust buffers on save again (with the language server), as `rust-format-on-save` has no effect in `rust-ts-mode`.
- [#1470](https://github.com/bbatsov/prelude/pull/1470): Fix `prelude-go`'s key bindings, format on save and `super-save` exemption not working in `go-ts-mode`.
- [#1472](https://github.com/bbatsov/prelude/pull/1472): Give JSON buffers the same setup in both JSON modes, with a `prelude-json-mode-hook` of their own (`json-ts-mode` buffers got none of it, and neither did `js-json-mode` ones on newer Emacsen).
- [#1472](https://github.com/bbatsov/prelude/pull/1472): Don't run `prelude-prog-mode-hook` twice in CSS and `web-mode` buffers.
- [#1473](https://github.com/bbatsov/prelude/pull/1473): Fix SCSS files opening in `fundamental-mode` with `prelude-scss` enabled: the module installed the `scss-mode` package from MELPA, which no longer loads, and now uses the `scss-mode` that comes with Emacs.
- [#1473](https://github.com/bbatsov/prelude/pull/1473): Don't break `fsharp-mode` when `eglot-fsharp` failed to install.
- [#1473](https://github.com/bbatsov/prelude/pull/1473): Add the missing `prelude-ocaml-mode-hook`, and run `prelude-ts-mode-hook` in `typescript-mode` buffers as well.
- [#1475](https://github.com/bbatsov/prelude/pull/1475): Open JavaScript files in `js-ts-mode` when its grammar is installed: `.js` files are associated with `javascript-mode`, an alias Prelude didn't remap, so they always ended up in `js-mode`. `.mjs` and `.cjs` files are recognized as JavaScript as well.
- [#1475](https://github.com/bbatsov/prelude/pull/1475): Install the `jsdoc` grammar together with the `javascript` one on Emacs 31+, where `js-ts-mode` fails to start without it.
- [#1476](https://github.com/bbatsov/prelude/pull/1476): Install tree-sitter grammars that work with the modes of the Emacs you're running: the pinned versions of the Go, JavaScript and TypeScript grammars were too new for Emacs 29 and 30 (breaking parts of their highlighting), the latest Swift grammar doesn't work with `swift-ts-mode`, and the YAML one could fail to build.
- [#1476](https://github.com/bbatsov/prelude/pull/1476): Don't try to install `heex-ts-mode` in `prelude-elixir` on Emacs 29, where it isn't available (it's built into Emacs 30+).
- [#1477](https://github.com/bbatsov/prelude/pull/1477): Fix Eglot failing with `void-function eglot-server-capable` on Emacs 29 as soon as a language server connects (`flycheck-eglot` relies on a function that Eglot got in Emacs 30).
- [#1480](https://github.com/bbatsov/prelude/pull/1480): Stop Emacs from loading an outdated `init.elc` or `early-init.elc` instead of Prelude's current init files (it prefers them even when the sources are newer). `prelude-update` no longer compiles the two, and compiled copies left by older versions are removed on startup.
- [#1480](https://github.com/bbatsov/prelude/pull/1480): Fix several problems in the installer: it deleted the existing config even when backing it up had failed, never reinstated the old files it promised to keep (on macOS it couldn't at all), byte-compiled the core in a way that kept packages from being installed, and printed garbage with `-c` under some shells.
- [#1248](https://github.com/bbatsov/prelude/issues/1248): Make the installer leave `~/.emacs` alone when Prelude is installed somewhere other than `~/.emacs.d`.
- [#1481](https://github.com/bbatsov/prelude/pull/1481): Fix killing a rectangular region (`C-x SPC` followed by `C-w`), which killed the text between point and mark instead.
- [#1481](https://github.com/bbatsov/prelude/pull/1481): Don't re-indent code yanked into Python buffers using `python-ts-mode` (and other modes derived from the ones in `prelude-indent-sensitive-modes`, which now covers Haskell and F# as well).
- [#1481](https://github.com/bbatsov/prelude/pull/1481): Don't show the welcome screen next to a new file when Emacs is started with the name of a file that doesn't exist yet.
- [#1481](https://github.com/bbatsov/prelude/pull/1481): Fix a few automatic package installs: opening an SCSS file installed a broken `scss-mode` over the built-in one, any file under a `Caskroom` directory installed `cask-mode`, and Python type stubs (`.pyi`) were taken for Cython.
- [#1481](https://github.com/bbatsov/prelude/pull/1481): Stop pointing all projects at a single Projectile cache file (Projectile keeps a cache per project these days).
- [#1332](https://github.com/bbatsov/prelude/issues/1332): Stop `prelude-mode` from intercepting `C-a` and its other editing keys in terminal emulators (`ansi-term`, `vterm`, `eat`), where they never reached the shell.
- [#1296](https://github.com/bbatsov/prelude/issues/1296): Don't have Flycheck flag every Prelude variable and function used in a personal config file as undefined.

## 2.1.0 (2026-03-29)

### New features

- Add `cmake-mode` package to `prelude-c`.
- Set `c-ts-mode-indent-style` to `k&r` so tree-sitter C/C++ modes match the classic cc-mode indentation style.
- Add `auto-mode-alist` entries for Eask and Eldev build tool files in `prelude-emacs-lisp`.
- Add Super-based (`s-`) keybindings on macOS for smartparens structural editing commands (splice, slurp, barf, splice-killing).

### Changes

- **Switch smartparens from paredit to default keybinding set.** This frees `M-s` (was `sp-splice-sexp`, shadowed `search-map`) and `M-?` (was `sp-convolute-sexp`, shadowed `xref-find-references`). Splice is now on `M-D`, convolute is unbound, splice-killing moves to `C-M-<backspace>` / `C-M-<delete>`.
- Convert all language modules to `use-package`: C, Clojure, Common Lisp, CSS, Dart, Elixir, Erlang, Emacs Lisp, F#, Go, Haskell, LaTeX, Lua, OCaml, Racket, Ruby, Rust, Scala, Scheme, SCSS, Web, YAML.
- Remove `prelude-coffee` module (CoffeeScript is no longer actively developed). The module file is kept as a deprecation stub so existing configs don't break.
- Remove `dune` and `utop` packages from `prelude-ocaml` (both are provided by `neocaml` out of the box).
- Load `ocaml-eglot` in `prelude-ocaml` only when `prelude-lsp-client` is set to `eglot`.
- Default to SBCL on all platforms in `prelude-common-lisp` (was Clozure CL on macOS).
- Disable `slime-enable-evaluate-in-emacs` by default in `prelude-common-lisp` (security risk).
- Add missing `prelude-lsp-enable` call in `prelude-fsharp`.
- Guard `eglot-fsharp` behind `prelude-lsp-client` check in `prelude-fsharp`.
- Use `major-mode-remap-alist` instead of `defalias` for cperl-mode in `prelude-perl`.
- Remove `C-c C-l` binding from `prelude-lua` (conflicts with Eglot prefix).
- Load `company-auctex` only when company is present in `prelude-latex`.
- Move Makefile configuration from `prelude-c` to `prelude-programming` (benefits all programming modules).
- Install `dart-mode` explicitly in `prelude-dart` instead of relying on auto-install.

### Bugs fixed

- [#1400](https://github.com/bbatsov/prelude/issues/1400): Fix `M-s` (`search-map`) and `M-?` (`xref-find-references`) being shadowed by smartparens paredit keybindings.

## 2.0.0 (2026-03-26)

### New features

- **Prelude now requires Emacs 29.1 or newer.** This unlocks built-in tree-sitter, Eglot, `use-package`, and many other modern Emacs features.
- Add `prelude-lsp-client` user option to switch between Eglot (default, built-in) and lsp-mode as the LSP client. All language modules use the configured client automatically.
- Add tree-sitter support for C/C++, Go, Python, JavaScript, TypeScript (including TSX), Ruby, Elixir, Shell, YAML, and CSS. Tree-sitter modes are used automatically when the grammar is available, with graceful fallback to legacy modes.
- Add `prelude-treesit-remap` helper to safely remap modes to their tree-sitter equivalents, with proper guards for grammar availability.
- Add LSP support to Python, JavaScript, TypeScript, C/C++, Ruby, Elixir, Haskell, Lua, and Erlang modules.
- Add Eglot keybindings under `C-c C-l` prefix (rename, code actions, format, organize imports), consistent with lsp-mode bindings.
- Add `prelude-vertico` module. Vertico is a simpler alternative to `ivy-mode` and supersedes Selectrum.
- Add `marginalia` to `prelude-vertico` for rich annotations in the minibuffer.
- Add a Racket module.
- Add a Lua module.
- Add a F# module.
- Add a module to enable Literate Programming (`prelude-literal-programming.el`).
- [PR 1432](https://github.com/bbatsov/prelude/pull/1432): Allow directories of custom Emacs Lisp files in `personal/preload`.
- Add `prelude-projectile` user option, allowing Projectile integration to be disabled.
- Add `prelude-hippie-expand` user option, allowing hippie-expand support to be disabled.
- [#1421](https://github.com/bbatsov/prelude/issues/1421): Make it possible to configure the TypeScript format action using `prelude-ts-format-action`.
- [#1354](https://github.com/bbatsov/prelude/issues/1354): Remove default `C--` and `C-+` keybindings to increase/decrease the font size.
- Enable `pixel-scroll-precision-mode` for smooth scrolling on graphical displays.
- Enable `isearch-lazy-count` for showing match counts during search and query-replace.
- Enable `org-habits`.
- Add `org-capture` binding (`C-c c`) and enable `org-indent-mode` by default.
- Neatly track `TODO` state changes in a drawer (LOGBOOK), thereby improving readability.
- Enable `subword-mode` in C, Dart, F#, LaTeX, OCaml, and Perl modules for better CamelCase navigation.
- Add indentation defaults and modern template file extensions (`.vue`, `.svelte`, `.astro`, etc.) to `prelude-web`.
- Auto-install `racket-mode` if needed.

### Changes

- Modernize `prelude-ocaml` to use `neocaml` (tree-sitter), `eglot`, and `ocaml-eglot` instead of `tuareg`, `merlin`, and `flycheck-ocaml`.
- Modernize `prelude-python` to use LSP instead of `anaconda-mode`.
- Modernize `prelude-js` to use `js-ts-mode` and LSP instead of `js2-mode`.
- Modernize `prelude-ts` to use `typescript-ts-mode` and LSP instead of `tide`.
- Rename `prelude-lsp` module to `prelude-lsp-mode` for clarity. It is now loaded on demand only when `prelude-lsp-client` is set to `lsp-mode`.
- Remove `prelude-selectrum` module (Selectrum is unmaintained, use Vertico instead).
- Remove `nlinum` package in favor of built-in `display-line-numbers-mode`.
- Remove `anzu` package in favor of built-in `isearch-lazy-count`.
- Remove `epl` package in favor of built-in `package-upgrade-all` / `package-upgrade`.
- Remove `alchemist` from `prelude-elixir` (unmaintained since 2018).
- Remove `go-projectile` from `prelude-go` (unmaintained).
- Replace `defadvice` with modern `define-advice` / `advice-add` throughout.
- Replace deprecated `point-at-eol` with `line-end-position` in `prelude-evil`.
- Replace deprecated `linum-mode` ex command with `display-line-numbers-mode` in `prelude-evil`.
- Remove obsolete magit keymaps from `prelude-evil` (`magit-commit-mode-map`, `magit-branch-manager-mode-map`).
- Switch SLIME completion from `slime-fuzzy` to `slime-flex` in `prelude-common-lisp`.
- Use `use-short-answers` instead of `fset` hack for y/n prompts.
- Use built-in `ansi-color-compilation-filter` for compilation buffer colorization.
- Use built-in `use-package` (no longer installed as a package).
- Use `use-package` to defer loading of interactive packages, improving startup time.
- Remove redundant `require` calls to improve startup time.
- Remove `C-x p` binding to `proced` (conflicts with built-in `project.el` prefix).
- Bind `C-x C-m` to `execute-extended-command` instead of `smex`.
- Remove redundant addition to `auto-mode-alist` for Markdown
  (note: this reverts the default to `markdown-mode` instead of `gfm-mode`).
- Bind all essential `avy` commands to their recommended keybindings.
- Remove `company-lsp`.
- Replace `yank-pop` key-binding to `counsel-yank-pop` for `ivy-mode`.
- Replace prelude-go backend with `lsp` instead of unmaintained tools.
- Restore format-on-save for Go via `gofmt-before-save`.
- Use `rust-analyzer` as language server for prelude-rust and provide nicer syntax highlighting with `tree-sitter`.
- Add `prelude-undo-tree` custom variable: allows user disable
  undo-tree integration. Enabled by default to maintain backward-compatibility.
- Disable ERC module by default and remove Freenode reference.
- Use HTTPS for MELPA on all platforms.
- Guard `set-fontset-font` with `display-graphic-p` to avoid errors in terminal Emacs.
- Switch docs theme from ReadTheDocs to Material for MkDocs.
- CI now tests on multiple Emacs versions (29.x, 30.x).

### Bugs fixed

- Fix hook variable name mismatch in `prelude-fsharp` (`prelude-sharp-mode-hook` → `prelude-fsharp-mode-hook`).
- Fix duplicate `zlogin` entry and wrong comment in `prelude-shell`.
- Fix wrong header comment in `prelude-ocaml` (said "Perl").
- Fix spurious `(interactive)` in `prelude-ts-mode-defaults`.
- Fix duplicate `auto-mode-alist` entries in `prelude-racket`.
- [#1445](https://github.com/bbatsov/prelude/issues/1445): Fix `prelude-rust` failing to load due to tree-sitter dependency issue.
- Fix wrong hook name in `prelude-rust`.
- [PR 1433](https://github.com/bbatsov/prelude/pull/1433): Remove a duplicate `when` call in `modules/prelude-helm-everywhere.el` causing an emacs init error when `prelude-helm-everywhere` is enabled.
- Fix `company` still being visible in the mode line.
- [#1335](https://github.com/bbatsov/prelude/issues/1335): Workaround
  for `which-key` bug causing display issues in clients to `emacs --daemon`.
- Fix **Edit on GitHub** link in ReadTheDocs site.
- Fix fall back to sample `prelude-modules.el` not working if user has installed to non-default location.
- Stop requiring `helm-config` since upstream has removed the module.
- Turn off `super-save` in `rust-mode` to prevent severe hangs during autocomplete.
- Update `prelude-dart.el` to use `lsp-dart-dap-setup` instead of deprecated `dap-dart-setup` function.
- Fix stale references and broken config across modules.

## 1.1.0 (2021-02-14)

### New features

- Enable `nlinum-mode` or `display-line-numbers-mode` by default. Can be disabled by setting `prelude-minimalistic-ui` to `t`.
- Enable site-wide installation for Prelude.
- Auto-installs `julia-mode` if needed.
- Auto-install `adoc-mode` for AsciiDoc files.
- Add the `ag` package. It provides a nice alternative to `grep` and has nice Projectile integration.
- Added additional configuration modules for WSL (`prelude-wsl`) and Windows (`prelude-windows`).
- Add `prelude-selectrum` module. Selectrum a simpler alternative to `ivy-mode`.

### Changes

- [#1292](https://github.com/bbatsov/prelude/issues/1292): Add `prelude-python-mode-set-encoding-automatically` defcustom inn `prelude-python.el` module with nil default value.
- [#1278](https://github.com/bbatsov/prelude/issues/1278): Don't disable `menu-bar-mode` unless `prelude-minimalistic-ui` is enabled.
- [#1277](https://github.com/bbatsov/prelude/issues/1277): Make it possible to disable the creation of `Super`-based keybindings via `prelude-super-keybindings`.
- Removed deprecated alias `prelude-ensure-module-deps`.
- Remove `prelude-fullscreen`, as these days people can use `toggle-frame-fullscreen` instead. (it was introduced in Emacs 24.4)
- Removed `beacon-mode`.
- Added `transient/` to `.gitignore`.
- Fallback to `sample/prelude-modules.el` in the absence of a `prelude-modules.el` in one's personal folder.
- [Ruby] Don't auto-insert coding comments.
- Hide (via `diminish`) `editorconfig-mode`, `super-save`, `company`, `abbrev` and `ivy` in the modeline.
- Use `lsp-workspace-restart` function instead of deprecated `lsp-restart-workspace`.
- Bind all online search commands under `C-c C-/` to avoid a conflict with `counsel-git` or `magit-file-dispatch`.
- Bound `magit-file-mode` to `C-c g` (it's also bound to `C-c M-g` if you decide to utilize this keybinding for something else.
- Added `.cache/` and `lsp-session*` to `.gitignore`. These are generated by `lsp`.

### Bugs fixed

- [#1445](https://github.com/bbatsov/prelude/issues/1445): prelude-rust fails to load due to tree-sitter dependency issue
- [#1302](https://github.com/bbatsov/prelude/issues/1302): `C-a` should be bound to `org-beginning-of-line` in org-mode buffers.

## 1.0.0 (2020-09-15)

Initial "stable" release after 9 years of development.
