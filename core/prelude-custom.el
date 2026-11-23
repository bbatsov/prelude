;;; prelude-custom.el --- Emacs Prelude: Prelude's customizable variables.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Refinements of the core editing experience in Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; customize
(defgroup prelude nil
  "Emacs Prelude configuration."
  :prefix "prelude-"
  :group 'convenience)

(defcustom prelude-minimalistic-ui nil
  "Controls whether to display the menu-bar and line numbers.
Note that the toolbar is always hidden regardless of this setting."
  :type 'boolean
  :group 'prelude
  :package-version '(prelude . "1.1"))

(defcustom prelude-super-keybindings t
  "Controls whether to use the Super key in keybindings.
They can be problematic in some operating systems (e.g. Windows)
or desktop environments that make heavy use of them."
  :type 'boolean
  :group 'prelude
  :package-version '(prelude . "1.1"))

(defcustom prelude-auto-save t
  "Non-nil values enable Prelude's auto save.
Buffers visiting files are saved when you switch to another buffer or
window, and when Emacs loses focus (see `super-save-mode')."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-guru t
  "Non-nil values enable `guru-mode'."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-whitespace t
  "Non-nil values enable Prelude's whitespace visualization."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-undo-tree t
  "Non-nil values enable Prelude's undo-tree integration."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-clean-whitespace-on-save t
  "Cleanup whitespace from file before it's saved.
Will only occur if `prelude-whitespace' is also enabled."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-whitespace-cleanup-style 'whitespace-cleanup
  "How whitespace is cleaned up on save when enabled.
`whitespace-cleanup' (the default) tidies the whole file, which can
produce noisy whitespace-only diffs in projects that aren't already
clean.  `ws-butler' instead trims only the lines you actually edited,
leaving the rest of the file untouched (installed on demand)."
  :type '(choice (const :tag "whitespace-cleanup (whole file)" whitespace-cleanup)
                 (const :tag "ws-butler (edited lines only)" ws-butler))
  :group 'prelude)

(defcustom prelude-flyspell t
  "Non-nil values enable Prelude's spell checking support."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-spell-checker 'flyspell
  "The spell checker to use when `prelude-flyspell' is enabled.
`flyspell' is the classic built-in checker (it needs an external
aspell/hunspell binary).  `jinx' is a faster, libenchant-based
checker that only checks the visible portion of the buffer, but it
requires libenchant and a C compiler to build its native module."
  :type '(choice (const :tag "Flyspell (built-in)" flyspell)
                 (const :tag "Jinx (libenchant)" jinx))
  :group 'prelude)

(defvar prelude-personal-dir)

(defcustom prelude-user-init-file (or (bound-and-true-p prelude-personal-dir)
                                      (expand-file-name "personal/" user-emacs-directory))
  "The file or directory that holds your personal configuration.
It's what `prelude-find-user-init-file' takes you to.  The default is
the personal folder, where Prelude recommends that you keep your
customizations.  If most of them are in one file, point this at it,
e.g. by putting `(setq prelude-user-init-file load-file-name)' in that
file."
  :type 'file
  :group 'prelude)

(defcustom prelude-indent-sensitive-modes
  '(conf-mode fsharp-mode haml-mode haskell-mode python-base-mode
              slim-mode yaml-mode yaml-ts-mode)
  "Modes for which auto-indenting is suppressed.
That's the modes of languages where the indentation carries meaning, so
it can't be worked out from the code.  Modes derived from these are
covered as well."
  :type '(repeat symbol)
  :group 'prelude)

(defcustom prelude-format-on-save t
  "Whether modules should format a file before it's saved.
Applies to the modules that format on save: Go and Rust."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-yank-indent-modes '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here."
  :type '(repeat symbol)
  :group 'prelude)

(defcustom prelude-yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur."
  :type 'natnum
  :group 'prelude)

(defcustom prelude-welcome-screen t
  "Non-nil values show Prelude's welcome screen on startup.
The welcome screen is skipped when Emacs is started with file
arguments (e.g. `emacs foo.rb')."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-theme 'zenburn
  "The default color theme, change this in your /personal/preload config."
  :type '(choice (const :tag "None" nil) symbol)
  :group 'prelude)

(defcustom prelude-projectile t
  "Non-nil values enable Prelude's Projectile integration."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-hippie-expand t
  "Non-nil values enable Prelude's hippie-expand support."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-smartparens t
  "Non-nil values enable Prelude's smartparens integration.
Set this in `personal/preload', since it takes effect when the core loads."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-treesit-auto-install 'ask
  "Whether to install missing tree-sitter grammars when they're needed.
When you open a file whose mode has a tree-sitter variant and the
grammar for its language isn't installed, Prelude can install it:
`ask' asks first, `always' installs it right away and nil never does.
Without the grammar the classic major mode is used."
  :type '(choice (const :tag "Ask first" ask)
                 (const :tag "Always" always)
                 (const :tag "Never" nil))
  :group 'prelude)

(defcustom prelude-lsp-client 'eglot
  "The LSP client to use in programming modes.
Eglot is built into Emacs 29+ and requires no extra packages.
Set to `lsp-mode' if you prefer lsp-mode with lsp-ui (peek
definitions, sideline diagnostics, etc.)."
  :type '(choice (const :tag "Eglot (built-in)" eglot)
                 (const :tag "LSP Mode" lsp-mode))
  :group 'prelude)

(provide 'prelude-custom)

;;; prelude-custom.el ends here
