# Prelude Programming

Prelude's programming module enables some additional functionality
for `prog-mode` - the parent mode for all major programming
modes in Emacs.

Here are some features it provides:

- spell-checking of comments (via `flyspell-prog-mode`)
- auto-pairing of delimiters like parentheses (via `smartparens`)
- visual cues for whitespace (via `whitespace-mode`)
- highlighting code annotations (via `hl-todo`)
- linter integration (via `flycheck`)
- showing current definition name in the modeline
  (via `which-func`)

Most of this boils down to enabling a bunch of minor modes in
`prog-mode-hook`.

## Smartparens Keybindings

Prelude uses smartparens' own default keybinding set (not the
paredit compatibility set). This avoids conflicts with standard
Emacs key prefixes:

- `M-D` for `sp-splice-sexp` (instead of paredit's `M-s`,
  which shadows the `search-map` prefix)
- `sp-convolute-sexp` is unbound (instead of paredit's `M-?`,
  which shadows `xref-find-references`)
- `C-M-<backspace>` / `C-M-<delete>` for splice-killing
  (instead of paredit's `M-<up>` / `M-<down>`)

On macOS, Super-based alternatives are also available:

- `s-s` -- splice
- `s-<right>` / `s-<left>` -- slurp / barf
- `s-<up>` / `s-<down>` -- splice-killing

### Using paredit instead of smartparens

If you prefer paredit over smartparens, be aware that paredit's
default keybindings have several conflicts with modern Emacs:

- `M-s` (`paredit-splice-sexp`) shadows the `search-map`
  prefix used by `occur`, `isearch-forward-symbol-at-point`,
  and consult commands like `consult-line` and
  `consult-ripgrep`.
- `M-?` (`paredit-convolute-sexp`) shadows
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
