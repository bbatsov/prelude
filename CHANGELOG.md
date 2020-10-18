# Change log

## master (unreleased)

### New features

* Enable `nlinum-mode` by default. Can be disabled by setting `prelude-minimalistic-ui` to `nil`.
* Enable site-wide installation for Prelude.
* Auto-installs `julia-mode` if needed.
* Auto-install `adoc-mode` for AsciiDoc files.
* Add the `ag` package. It provides a nice alternative to `grep` and has nice Projectile integration.
* Added additional configuration modules for WSL (`prelude-wsl`) and Windows (`prelude-windows`).

### Changes

* [#1292](https://github.com/bbatsov/prelude/issues/1292): Add `prelude-python-mode-set-encoding-automatically` defcustom inn `prelude-python.el` module with nil default value.
* [#1278](https://github.com/bbatsov/prelude/issues/1278): Don't disable `menu-bar-mode` unless `prelude-minimalistic-ui` is enabled.
* [#1277](https://github.com/bbatsov/prelude/issues/1277): Make it possible to disable the creation of `Super`-based keybindings via `prelude-super-keybindings`.
* Removed deprecated alias `prelude-ensure-module-deps`.
* Remove `prelude-fullscreen`, as these days people can use `toggle-frame-fullscreen` instead. (it was introduced in Emacs 24.4)
* Removed `beacon-mode`.
* Added `transient/` to `.gitignore`.
* Fallback to `sample/prelude-modules.el` in the absence of a `prelude-modules.el` in one's personal folder.

### Bugs fixed

## 1.0.0 (2020-09-15)

Initial "stable" release after 9 years of development.
