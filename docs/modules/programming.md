# Prelude Programming

Prelude's programming module enables some additional functionality
for `prog-mode` - the parent mode for all major programming
modes in Emacs.

Here are some features it provides:

- spell-checking of comments (via `flyspell-prog-mode`)
- auto-pairing of delimiters like parentheses (via `smartparens`, or
  the built-in `electric-pair-mode` when `prelude-smartparens` is off)
- visual cues for whitespace (via `whitespace-mode`)
- highlighting code annotations (via `hl-todo`)
- linter integration (via `flycheck`), which also shows the
  diagnostics of Eglot (via `flycheck-eglot-mode`, a part of Flycheck)
- showing current definition name in the modeline
  (via `which-func`)
- nudging you towards the Emacs way of doing things (via `guru-mode`,
  which only warns)
- auto-filling of comments only, when `auto-fill-mode` is on

The module also sets up `makefile-mode`: tabs are used for indentation
there (as Makefiles require them) and aren't highlighted as a problem.

Most of this boils down to enabling a bunch of minor modes in
`prog-mode-hook`.

## Smartparens Keybindings

Prelude uses smartparens' own default keybinding set (not the
paredit compatibility set). This avoids conflicts with standard
Emacs key prefixes:

- <kbd>M-D</kbd> for `sp-splice-sexp` (instead of paredit's <kbd>M-s</kbd>,
  which shadows the `search-map` prefix)
- `sp-convolute-sexp` is unbound (instead of paredit's <kbd>M-?</kbd>,
  which shadows `xref-find-references`)
- <kbd>C-M-&lt;backspace&gt;</kbd> / <kbd>C-M-&lt;delete&gt;</kbd> for splice-killing
  (instead of paredit's <kbd>M-&lt;up&gt;</kbd> / <kbd>M-&lt;down&gt;</kbd>)

On macOS, Super-based alternatives are also available:

- <kbd>s-s</kbd> - splice
- <kbd>s-&lt;right&gt;</kbd> / <kbd>s-&lt;left&gt;</kbd> - slurp / barf
- <kbd>s-&lt;up&gt;</kbd> / <kbd>s-&lt;down&gt;</kbd> - splice-killing

### Using paredit instead of smartparens

If you prefer paredit over smartparens, be aware that paredit's
default keybindings have several conflicts with modern Emacs:

- <kbd>M-s</kbd> (`paredit-splice-sexp`) shadows the `search-map`
  prefix used by `occur`, `isearch-forward-symbol-at-point`,
  and consult commands like `consult-line` and
  `consult-ripgrep`.
- <kbd>M-?</kbd> (`paredit-convolute-sexp`) shadows
  `xref-find-references`, which is essential for LSP
  (Eglot/lsp-mode) users.

To fix these, add the following to your personal config:

```emacs-lisp
(with-eval-after-load 'paredit
  ;; Free M-s for search-map
  (define-key paredit-mode-map (kbd "M-s") nil)
  (define-key paredit-mode-map (kbd "M-D") #'paredit-splice-sexp)

  ;; Free M-? for xref-find-references
  (define-key paredit-mode-map (kbd "M-?") nil))
```
