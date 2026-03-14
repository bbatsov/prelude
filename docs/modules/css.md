# Prelude CSS

## CSS Mode

Emacs comes with CSS editing support through the built-in
`css-mode`. This module
adds a couple of enhancements on top of it:

- **Indent offset**: 2 spaces (`css-indent-offset` set to `2`)
- **Rainbow mode**:
  [rainbow-mode](https://elpa.gnu.org/packages/rainbow-mode.html)
  is enabled, which colorizes color strings (like `#ff0000` or
  `rgb(255, 0, 0)`) with the color they represent

The module also runs the `prelude-prog-mode-hook`, so you get the same baseline
programming features (whitespace visualization, smartparens, etc.) as in other
programming modes.
