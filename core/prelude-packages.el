;;; prelude-packages.el --- Emacs Prelude: default package selection.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Takes care of the automatic installation of all the packages required by
;; Emacs Prelude.  This module also adds a couple of package.el extensions
;; and provides functionality for auto-installing major modes on demand.

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
(require 'cl-lib)
(require 'package)
(require 'use-package)

;;;; Package setup and additional utility functions

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; load the pinned packages
(let ((prelude-pinned-packages-file (expand-file-name "prelude-pinned-packages.el" prelude-dir)))
  (if (file-exists-p prelude-pinned-packages-file)
      (load prelude-pinned-packages-file)))

(defun prelude--packages-activated-p (activated-dir)
  "Return non-nil if Emacs already activated the packages in `package-user-dir'.
Emacs activates the installed packages on its own before it loads
init.el, from ACTIVATED-DIR (the `package-user-dir' at that point)."
  (and (bound-and-true-p package--activated)
       (string= (file-truename (file-name-as-directory activated-dir))
                (file-truename (file-name-as-directory package-user-dir)))))

(let ((activated-dir package-user-dir))
  ;; set package-user-dir to be relative to Prelude install path
  (when prelude-override-package-user-dir
    (setq package-user-dir (expand-file-name "elpa" prelude-dir)))
  ;; Only activate the packages if Emacs hasn't (e.g. when
  ;; `package-enable-at-startup' is off, or Prelude isn't installed in
  ;; `user-emacs-directory').  The archive contents are read when
  ;; something gets installed.
  (unless (prelude--packages-activated-p activated-dir)
    (package-initialize)))

;; use-package is built-in since Emacs 29
(setq use-package-verbose t)

(defvar prelude-packages nil
  "Packages installed and managed by Prelude.
Prelude's `use-package' forms add the packages they ensure here as they
run, which is what `prelude-update-packages' and
`prelude-list-foreign-packages' go by.  Packages you add to it in
`personal/preload' are installed at startup as well.")

(defun prelude-packages-installed-p ()
  "Check if all packages in `prelude-packages' are installed."
  (cl-every #'package-installed-p prelude-packages))

(defvar prelude--package-archives-refreshed nil
  "Non-nil once `prelude-package-install' has refreshed the package archives.")

(defun prelude-package-install (package)
  "Install PACKAGE, refreshing the package archives and retrying on failure.

A stale package cache can list versions that are no longer on the
server (MELPA only keeps the latest build) or miss newly added
packages, and then installing fails.  The archives are refreshed at
most once per session."
  ;; A `use-package' :pin is only recorded when the form runs, after the
  ;; archives were read, so re-read them for the pin to take effect.
  (when (assq package package-pinned-packages)
    (package-read-all-archive-contents))
  (condition-case err
      (package-install package)
    (error
     (if prelude--package-archives-refreshed
         (signal (car err) (cdr err))
       (message "[Prelude] Failed to install %s, refreshing the package archives and retrying..." package)
       (package-refresh-contents)
       (setq prelude--package-archives-refreshed t)
       (package-install package)))))

(defun prelude-use-package-ensure (name args state &optional no-refresh)
  "Install the packages of a `use-package' form with `prelude-package-install'.
NAME, ARGS, STATE and NO-REFRESH are as for `use-package-ensure-elpa',
which still handles the pinned (package . archive) form."
  (dolist (ensure args)
    (let ((package (if (eq ensure t) (use-package-as-symbol name) ensure)))
      ;; If this ever runs while a `use-package' form is being compiled,
      ;; NAME is a symbol with position.  If that ends up in
      ;; `package-selected-packages', every later install fails.
      (when (and package (symbolp package))
        (setq package (bare-symbol package)))
      (cond
       ((null package))                 ; `:ensure nil'
       ((symbolp package)
        ;; Built-in packages (e.g. which-key on Emacs 30+) aren't
        ;; tracked, so `prelude-update-packages' doesn't replace them
        ;; with ELPA copies.
        (unless (package-built-in-p package)
          (add-to-list 'prelude-packages package))
        (unless (package-installed-p package)
          (condition-case-unless-debug err
              (prelude-package-install package)
            (error
             (display-warning 'prelude
                              (format "Failed to install %s: %s"
                                      package (error-message-string err))
                              :error)))))
       ;; the pinned (package . archive) form
       (t (use-package-ensure-elpa name (list ensure) state no-refresh))))))

(setq use-package-ensure-function #'prelude-use-package-ensure)

(defun prelude--use-package-ensure-at-load-time (handler &rest args)
  "Call the `use-package' :ensure HANDLER with ARGS as if not compiling.
When a file is byte-compiled, `use-package' ensures packages at compile
time and leaves nothing to do at load time, so compiled forms would
never install (or track) their packages, and conditions around them
wouldn't be respected."
  (let ((byte-compile-current-file nil))
    (apply handler args)))

(advice-add 'use-package-handler/:ensure :around
            #'prelude--use-package-ensure-at-load-time)

(defun prelude-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package prelude-packages)
    (add-to-list 'prelude-packages package))
  (unless (package-installed-p package)
    (prelude-package-install package)))

(defun prelude-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (with-suppressed-warnings ((obsolete prelude-require-package))
    (mapc #'prelude-require-package packages)))

(make-obsolete 'prelude-require-package
               "use `use-package' with `:ensure t' instead." "2.2.0")
(make-obsolete 'prelude-require-packages
               "use `use-package' with `:ensure t' instead." "2.2.0")

(defun prelude-install-packages ()
  "Install all packages listed in `prelude-packages'."
  (unless (prelude-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs Prelude is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    (setq prelude--package-archives-refreshed t)
    ;; install the missing packages
    (dolist (package prelude-packages)
      (unless (package-installed-p package)
        (prelude-package-install package)))))

;; run package installation
(prelude-install-packages)

(defun prelude-list-foreign-packages ()
  "Browse third-party packages not bundled with Prelude.

Behaves similarly to `package-list-packages', but shows only the packages that
are installed and are not in `prelude-packages'.  Useful for
removing unwanted packages."
  (interactive)
  (package-show-package-list
   (cl-set-difference package-activated-list prelude-packages)))

;;;; Auto-installation of major modes on demand

(defun prelude-auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present.  The file is opened in MODE."
  (add-to-list 'auto-mode-alist
               (cons extension
                     (lambda ()
                       (unless (package-installed-p package)
                         (prelude-package-install package))
                       (funcall mode)))))

(defvar prelude-auto-install-alist
  '(("\\.adoc\\'" adoc-mode adoc-mode)
    ("\\.clj\\'" clojure-mode clojure-mode)
    ("\\.cljc\\'" clojure-mode clojurec-mode)
    ("\\.cljs\\'" clojure-mode clojurescript-mode)
    ("\\.edn\\'" clojure-mode clojure-mode)
    ("\\.cmake\\'" cmake-mode cmake-mode)
    ("CMakeLists\\.txt\\'" cmake-mode cmake-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ("Cask" cask-mode cask-mode)
    ("\\.d\\'" d-mode d-mode)
    ("\\.dart\\'" dart-mode dart-mode)
    ("\\.elm\\'" elm-mode elm-mode)
    ("\\.ex\\'" elixir-mode elixir-mode)
    ("\\.exs\\'" elixir-mode elixir-mode)
    ("\\.elixir\\'" elixir-mode elixir-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.graphql\\'" graphql-mode graphql-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.jl\\'" julia-mode julia-mode)
    ("\\.kt\\'" kotlin-mode kotlin-mode)
    ("\\.kv\\'" kivy-mode kivy-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.markdown\\'" markdown-mode markdown-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.ml\\'" neocaml neocaml-mode)
    ("\\.pp\\'" puppet-mode puppet-mode)
    ("\\.php\\'" php-mode php-mode)
    ("\\.proto\\'" protobuf-mode protobuf-mode)
    ("\\.pyd\\'" cython-mode cython-mode)
    ("\\.pyi\\'" cython-mode cython-mode)
    ("\\.pyx\\'" cython-mode cython-mode)
    ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
    ("\\.rkt\\'" racket-mode racket-mode)
    ("\\.rs\\'" rust-ts-mode rust-ts-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.styl\\'" stylus-mode stylus-mode)
    ("\\.swift\\'" swift-mode swift-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.thrift\\'" thrift thrift-mode)
    ("Dockerfile\\'" dockerfile-mode dockerfile-mode)))

;; same with adoc-mode
(when (package-installed-p 'adoc-mode)
  (add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))
  (add-to-list 'auto-mode-alist '("\\.asciidoc\\'" . adoc-mode)))

;; and pkgbuild-mode
(when (package-installed-p 'pkgbuild-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode)))

;; build auto-install mappings
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (prelude-auto-install extension package mode))))
 prelude-auto-install-alist)

(provide 'prelude-packages)
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; prelude-packages.el ends here
