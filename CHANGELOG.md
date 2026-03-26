<!-- markdownlint-disable MD013 MD024 -->

# Change log

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
