;;; prelude-treesit.el --- Prelude's tree-sitter support. -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.1.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Prelude's modules use the tree-sitter based major modes whenever the
;; grammar for the language is available, and can install a missing
;; grammar the first time it's needed (see `prelude-treesit-auto-install').
;; If there's no grammar (and none gets installed), the classic major
;; mode is used instead.  This works the same way on every supported
;; Emacs version, whether the tree-sitter mode comes with Emacs or from
;; a package.

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

(require 'treesit nil t)

(defvar prelude-treesit-language-sources
  `((bash "https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3")
    (c "https://github.com/tree-sitter/tree-sitter-c" "v0.23.4")
    (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp"
             ;; csharp-ts-mode in Emacs 29 can't handle newer grammars
             ,(if (< emacs-major-version 30) "v0.20.0" "v0.23.1"))
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.4")
    (css "https://github.com/tree-sitter/tree-sitter-css" "v0.23.1")
    (elixir "https://github.com/elixir-lang/tree-sitter-elixir" "v0.3.3")
    (go "https://github.com/tree-sitter/tree-sitter-go"
        ,(if (< emacs-major-version 31) "v0.20.0" "v0.23.4"))
    (gomod "https://github.com/camdencheek/tree-sitter-go-mod" "v1.1.0")
    (heex "https://github.com/phoenixframework/tree-sitter-heex" "v0.7.0")
    (java "https://github.com/tree-sitter/tree-sitter-java" "v0.23.5")
    (javascript "https://github.com/tree-sitter/tree-sitter-javascript"
                ,(cond ((< emacs-major-version 30) "v0.19.0")
                       ((< emacs-major-version 31) "v0.20.1")
                       (t "v0.23.1")))
    (json "https://github.com/tree-sitter/tree-sitter-json" "v0.24.8")
    (python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6")
    (ruby "https://github.com/tree-sitter/tree-sitter-ruby" "v0.23.1")
    (rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.23.2")
    ;; the last release swift-ts-mode can work with
    (swift "https://github.com/alex-pinkus/tree-sitter-swift" "0.7.1-with-generated-files")
    (tsx "https://github.com/tree-sitter/tree-sitter-typescript"
         ,(if (< emacs-major-version 30) "v0.20.3" "v0.23.2") "tsx/src")
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                ,(if (< emacs-major-version 30) "v0.20.3" "v0.23.2") "typescript/src")
    (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml" "v0.7.0"))
  "Grammar recipes for the languages Prelude's modules use.
The format is the same as for `treesit-language-source-alist'.  A recipe
from here is only used when that variable has none for the language:
Emacs 31 ships recipes for the grammars of its own tree-sitter modes,
pinned to versions its tree-sitter library can load.  The versions here
are the releases closest to those, or the newest ones the tree-sitter
modes of older Emacsen can work with, which is why some of them depend
on the Emacs version.  Automatically installing whatever is the latest
version of a grammar can easily produce one that doesn't work.

test/prelude-grammar-test.el checks all of them, and CI runs it on
every supported Emacs.")

(defvar prelude-treesit-companion-grammars
  '((javascript jsdoc))
  "Alist of (LANG . GRAMMARS) for the extra grammars a language needs.
A companion grammar counts only in the Emacs versions whose tree-sitter
modes install the grammars they need themselves, and only if Emacs has
a recipe for it.  That's how we know that the mode can't do without it:
`js-ts-mode' in Emacs 31 doesn't start without the jsdoc grammar, while
older versions get by without it.")

(defvar prelude-treesit--languages nil
  "Alist of (LANG . TS-MODE) for every language set up by Prelude.")

(defvar prelude-treesit--declined nil
  "Languages whose grammar wasn't installed this session.
Either the user declined to install it, or installing it failed.")

(defun prelude-treesit--supported-p ()
  "Return non-nil if this Emacs was built with tree-sitter support."
  (and (fboundp 'treesit-available-p) (treesit-available-p)))

(defun prelude-treesit--recipe (lang ts-mode)
  "Return the grammar recipe for LANG, used by TS-MODE.
Prelude's recipe from `prelude-treesit-language-sources' is added to
`treesit-language-source-alist' unless that already has one."
  ;; Loading the mode registers the recipe Emacs has for it, if any.
  ;; Some modes warn when they're loaded without their grammar.
  (let ((def (symbol-function ts-mode))
        (warning-minimum-level :emergency)
        (warning-minimum-log-level :emergency))
    (when (autoloadp def)
      (ignore-errors (autoload-do-load def ts-mode))))
  (or (assq lang treesit-language-source-alist)
      (let ((recipe (assq lang prelude-treesit-language-sources)))
        (when recipe
          (push recipe treesit-language-source-alist))
        recipe)))

(defun prelude-treesit--install-grammar (lang ts-mode)
  "Install the grammar for LANG, used by TS-MODE.
Return non-nil if the grammar is available afterwards."
  (if (not (prelude-treesit--recipe lang ts-mode))
      (progn
        (message "[Prelude] Don't know where to get the %s tree-sitter grammar from" lang)
        nil)
    (condition-case err
        (progn
          (treesit-install-language-grammar lang)
          (treesit-language-available-p lang))
      (error
       (message "[Prelude] Couldn't install the %s tree-sitter grammar: %s"
                lang (error-message-string err))
       nil))))

(defun prelude-treesit--grammars (lang ts-mode)
  "Return the grammars TS-MODE needs: LANG and its companions.
See `prelude-treesit-companion-grammars'."
  (let ((companions (and (fboundp 'treesit-ensure-installed) ; Emacs 31+
                         (alist-get lang prelude-treesit-companion-grammars))))
    (when companions
      ;; this loads the mode, so we know which recipes Emacs has
      (prelude-treesit--recipe lang ts-mode))
    (cons lang
          (seq-filter (lambda (companion)
                        (assq companion treesit-language-source-alist))
                      companions))))

(defun prelude-treesit--ensure-grammar (lang ts-mode)
  "Return non-nil if the tree-sitter grammar for LANG can be used.
Install it first if it's missing, as configured by
`prelude-treesit-auto-install'.  TS-MODE is the mode that needs it.
The same goes for the companion grammars of LANG."
  (when (and (prelude-treesit--supported-p) (fboundp ts-mode))
    (let ((missing (seq-remove #'treesit-language-available-p
                               (prelude-treesit--grammars lang ts-mode))))
      (cond
       ((null missing) t)
       ((seq-intersection missing prelude-treesit--declined) nil)
       ;; Only install grammars when visiting a file, not when a mode
       ;; function gets called for other reasons (e.g. when Org fontifies
       ;; a source block, or for a new buffer named like a file).
       ((not buffer-file-name) nil)
       ((eq prelude-treesit-auto-install 'always)
        (prelude-treesit--try-install missing ts-mode))
       ((and (eq prelude-treesit-auto-install 'ask) (not noninteractive))
        (if (y-or-n-p (format "Install the tree-sitter %s for %s? "
                              (if (cdr missing) "grammars" "grammar")
                              (mapconcat #'symbol-name missing " and ")))
            (prelude-treesit--try-install missing ts-mode)
          (setq prelude-treesit--declined
                (append missing prelude-treesit--declined))
          nil))))))

(defun prelude-treesit--try-install (langs ts-mode)
  "Install the grammars for LANGS, used by TS-MODE, remembering a failure.
Return non-nil if all of them got installed."
  (seq-every-p (lambda (lang)
                 (or (prelude-treesit--install-grammar lang ts-mode)
                     (progn (push lang prelude-treesit--declined) nil)))
               langs))

(defun prelude-treesit-mode-function (lang ts-mode &optional fallback-mode fallback-package)
  "Return a major mode function that picks TS-MODE or FALLBACK-MODE.
TS-MODE is used when the tree-sitter grammar for LANG is available
\(see `prelude-treesit-auto-install' for installing a missing one),
FALLBACK-MODE otherwise.  If FALLBACK-MODE isn't defined and
FALLBACK-PACKAGE is given, that package is installed first.  Without a
\(defined) FALLBACK-MODE, `fundamental-mode' is used."
  (let ((name (intern (format "prelude-treesit-%s" ts-mode))))
    (defalias name
      (lambda ()
        (interactive)
        (if (prelude-treesit--ensure-grammar lang ts-mode)
            (funcall ts-mode)
          (when (and fallback-mode fallback-package (not (fboundp fallback-mode)))
            (prelude-package-install fallback-package))
          (funcall (if (and fallback-mode (fboundp fallback-mode))
                       fallback-mode
                     #'fundamental-mode))))
      (format "Use `%s' if the %s tree-sitter grammar is available, `%s' otherwise."
              ts-mode lang (or fallback-mode 'fundamental-mode)))
    (add-to-list 'prelude-treesit--languages (cons lang ts-mode))
    name))

(defun prelude-treesit-remap (lang old-mode new-mode)
  "Use NEW-MODE instead of OLD-MODE when LANG's tree-sitter grammar is available.
See `prelude-treesit-mode-function'."
  (add-to-list 'major-mode-remap-alist
               (cons old-mode (prelude-treesit-mode-function lang new-mode old-mode))))

(defun prelude-treesit-auto-mode (regexp lang ts-mode &optional fallback-mode fallback-package)
  "Open files matching REGEXP in TS-MODE when the grammar for LANG is available.
FALLBACK-MODE and FALLBACK-PACKAGE are as for
`prelude-treesit-mode-function'."
  (add-to-list 'auto-mode-alist
               (cons regexp (prelude-treesit-mode-function
                             lang ts-mode fallback-mode fallback-package))))

(defun prelude-treesit-install-grammars ()
  "Install the missing tree-sitter grammars of the enabled modules."
  (interactive)
  (unless (prelude-treesit--supported-p)
    (user-error "This Emacs was built without tree-sitter support"))
  (let ((installed nil))
    (pcase-dolist (`(,main . ,ts-mode) prelude-treesit--languages)
      (when (fboundp ts-mode)
        (dolist (lang (prelude-treesit--grammars main ts-mode))
          (when (and (not (treesit-language-available-p lang))
                     (prelude-treesit--install-grammar lang ts-mode))
            (setq prelude-treesit--declined (delq lang prelude-treesit--declined))
            (push lang installed)))))
    (message "[Prelude] %s"
             (if installed
                 (format "Installed tree-sitter grammars: %s"
                         (mapconcat #'symbol-name (nreverse installed) ", "))
               "No tree-sitter grammars to install"))))

(provide 'prelude-treesit)
;;; prelude-treesit.el ends here
