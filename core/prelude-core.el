;;; prelude-core.el --- Emacs Prelude: Core Prelude functions.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Here are the definitions of most of the general-purpose functions and
;; commands added by Prelude.  Some modules define additional module-specific
;; functions and commands.
;;
;; Note that many of the original core Prelude commands were extracted to the
;; crux package (Prelude installs it automatically).  Prelude's auto-save
;; functionality was extracted to the super-save package.

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

(defun prelude-search (query-url prompt)
  "Open the search url constructed with the QUERY-URL.
PROMPT sets the `read-string prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if mark-active
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt))))))

(defmacro prelude-install-search-engine (search-engine-name search-engine-url search-engine-prompt)
  "Define `prelude-SEARCH-ENGINE-NAME', a command searching SEARCH-ENGINE-URL.
The command prompts with SEARCH-ENGINE-PROMPT for the query, defaulting
to the active region."
  `(defun ,(intern (format "prelude-%s" search-engine-name)) ()
       ,(format "Search %s with a query or region if any." search-engine-name)
       (interactive)
       (prelude-search ,search-engine-url ,search-engine-prompt)))

(prelude-install-search-engine "google"     "http://www.google.com/search?q="              "Google: ")
(prelude-install-search-engine "youtube"    "http://www.youtube.com/results?search_query=" "Search YouTube: ")
(prelude-install-search-engine "github"     "https://github.com/search?q="                 "Search GitHub: ")
(prelude-install-search-engine "duckduckgo" "https://duckduckgo.com/?t=lm&q="              "Search DuckDuckGo: ")

(defun prelude-delete-init-bytecode ()
  "Delete the compiled versions of Prelude's init.el and early-init.el.
Return the ones that were older than their sources.

Emacs loads init.elc and early-init.elc in preference to the sources,
even when those are newer.  `load-prefer-newer' is of no help there, as
it's set by the very files in question.  A compiled init file left
behind by an older Prelude (or by an update that didn't go through
`prelude-update') would therefore shadow the current one forever, so
Prelude doesn't keep them around at all."
  (let ((stale nil))
    (dolist (name '("init" "early-init"))
      (let ((source (expand-file-name (concat name ".el") prelude-dir))
            (compiled (expand-file-name (concat name ".elc") prelude-dir)))
        (when (file-exists-p compiled)
          (when (file-newer-than-file-p source compiled)
            (push compiled stale))
          ;; We might not be allowed to, e.g. in a system-wide install.
          ;; That's no reason to fail to start.
          (condition-case err
              (delete-file compiled)
            (file-error
             (display-warning
              'prelude
              (format "Couldn't delete %s, which Emacs loads instead of %s: %s"
                      compiled (file-name-nondirectory source)
                      (error-message-string err))))))))
    (nreverse stale)))

;; This runs on every startup.  Even if a stale init.elc was loaded, it
;; has `load-prefer-newer' set, so we get here through the current
;; source of this file.
(let ((stale (prelude-delete-init-bytecode)))
  (when stale
    (display-warning
     'prelude
     (format "Removed outdated byte-code that Emacs was loading instead of Prelude's current init files (%s).  Restart Emacs to start with the right ones."
             (mapconcat #'file-name-nondirectory stale ", ")))))

(defun prelude-recompile-init ()
  "Byte-compile Prelude's core, modules and vendored code again.
init.el and early-init.el are never compiled, see
`prelude-delete-init-bytecode'.  Your personal config isn't either, as
it's loaded from its source files."
  (interactive)
  (prelude-delete-init-bytecode)
  (dolist (dir (list prelude-core-dir prelude-modules-dir prelude-vendor-dir))
    (when (file-directory-p dir)
      (byte-recompile-directory dir 0))))

(defvar prelude-tips
  '("Press <C-c o> to open a file with external program."
    "Press <C-c p f> to navigate a project's files."
    "Press <s-r> to open a recently visited file."
    "Press <C-c p s g> to run grep on a project."
    "Press <C-c p p> to switch between projects."
    "Press <C-=> to expand the selected region."
    "Press <C-c C-/ g> to search in Google."
    "Press <C-c C-/ h> to search in GitHub."
    "Press <C-c C-/ y> to search in YouTube."
    "Press <C-c C-/ d> to search in DuckDuckGo."
    "Press <C-c r> to rename the current buffer and the file it's visiting if any."
    "Press <C-c t> to open a terminal in Emacs."
    "Press <C-c k> to kill all the buffers, but the active one."
    "Press <C-x g> to run magit-status."
    "Press <C-c D> to delete the current file and buffer."
    "Press <C-c s> to swap two windows."
    "Press <S-RET> or <M-o> to open a line beneath the current one."
    "Press <s-o> to open a line above the current one."
    "Press <C-c C-z> in a Elisp buffer to launch an interactive Elisp shell."
    "Press <C-Backspace> to kill a line backwards."
    "Press <C-S-Backspace> or <s-k> to kill the whole line."
    "Press <s-j> or <C-^> to join lines."
    "Press <s-.> or <C-c v> to jump to the start of a word in any visible window."
    "Press <f12> to toggle the menu bar."
    "Explore the Prelude menu to find out about some of Prelude extensions to Emacs."
    "Access the official Emacs manual by pressing <C-h r>."))

(defun prelude-tip-of-the-day ()
  "Display a random entry from `prelude-tips'."
  (interactive)
  (when (and prelude-tips (not (window-minibuffer-p)))
    ;; pick a new random seed
    (random t)
    (message
     (concat "Prelude tip: " (nth (random (length prelude-tips)) prelude-tips)))))

(defun prelude-eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.

    If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

(defun prelude-fetch-shell-environment-p ()
  "Return non-nil if Emacs needs to get its environment from the shell.
That's the case for GUI frames and daemons, which don't inherit it the
way Emacs started from a terminal does."
  (or (memq window-system '(mac ns x pgtk)) (daemonp)))

(defun prelude-update ()
  "Update Prelude to its latest version."
  (interactive)
  (when (y-or-n-p "Do you want to update Prelude? ")
    (message "Updating installed packages...")
    (package-upgrade-all)
    (message "Updating Prelude...")
    (let ((default-directory prelude-dir)
          (output (get-buffer-create "*prelude-update*")))
      (with-current-buffer output (erase-buffer))
      (unless (zerop (call-process "git" nil output nil "pull" "--ff-only"))
        (display-buffer output)
        (user-error "Updating Prelude with `git pull --ff-only' failed, see *prelude-update* for details")))
    (prelude-recompile-init)
    (message "Update finished. Restart Emacs to complete the process.")))

(defun prelude-update-packages (&optional arg)
  "Update Prelude's packages.
This covers every package Prelude installs (see `prelude-packages').

With a prefix ARG updates all installed packages."
  (interactive "P")
  (when (y-or-n-p "Do you want to update Prelude's packages? ")
    (if arg
        (package-upgrade-all)
      (package-refresh-contents)
      (dolist (package prelude-packages)
        (when (package-installed-p package)
          (package-upgrade package))))
    (message "Update finished. Restart Emacs to complete the process.")))

(defun prelude-wrap-with (s)
  "Create a wrapper function for smartparens using S."
  `(lambda (&optional arg)
     (interactive "P")
     (sp-wrap-with-pair ,s)))

(defvar prelude-conflicting-modules
  '((prelude-ido prelude-ivy prelude-vertico prelude-helm)
    (prelude-company prelude-corfu))
  "Groups of modules that do the same job and shouldn't be enabled together.")

(defun prelude-check-module-conflicts ()
  "Warn about enabled modules that conflict with each other.
See `prelude-conflicting-modules'."
  (dolist (group prelude-conflicting-modules)
    (let ((enabled (seq-filter #'featurep group)))
      (when (cdr enabled)
        (display-warning
         'prelude
         (format "Modules %s do the same job, keep only one of them in %s"
                 (mapconcat #'symbol-name enabled ", ")
                 prelude-modules-file))))))

(defvar eglot-server-programs)
(defvar lsp-enable-suggest-server-download)
(defvar lsp-warn-no-matched-clients)

(defvar prelude-lsp--reported-modes nil
  "Major modes we've already reported a missing language server for.")

(defun prelude-lsp--eglot-contact ()
  "Return the `eglot-server-programs' entry for the current major mode."
  (seq-some
   (pcase-lambda (`(,modes . ,contact))
     ;; MODES is a mode, a (MODE :language-id ID) spec, or a list of those
     (when (seq-some (lambda (mode)
                       (derived-mode-p (if (consp mode) (car mode) mode)))
                     (if (or (symbolp modes) (keywordp (cadr modes)))
                         (list modes)
                       modes))
       contact))
   eglot-server-programs))

(defun prelude-lsp--eglot-missing-server ()
  "Return a description of what's missing if Eglot can't start a server here.
Return nil if the language server of the current buffer seems to be
installed, or if there's no way to tell without starting it."
  (require 'eglot)
  (let ((contact (prelude-lsp--eglot-contact)))
    (condition-case err
        (progn
          ;; `eglot-alternatives' gives us a function that picks the
          ;; first of several servers that's installed, and signals an
          ;; error if none of them is.  Named functions are the entry
          ;; points of packages that install the server on demand, so
          ;; those are left alone.
          (when (and (functionp contact) (not (symbolp contact)))
            (setq contact
                  (if (eql (cdr (func-arity contact)) 1)
                      (funcall contact nil)
                    ;; Eglot 1.17+ passes the project as well
                    (funcall contact nil
                             (or (project-current)
                                 (cons 'transient default-directory))))))
          ;; skip the class of the server
          (when (and (consp contact) (symbolp (car contact))
                     (not (keywordp (car contact))))
            (setq contact (cdr contact)))
          (cond
           ((null contact)
            (format-message "Eglot doesn't know a language server for `%s'"
                            major-mode))
           ;; (PROGRAM ARGS...), as opposed to (HOST PORT)
           ((and (consp contact) (stringp (car contact))
                 (not (integerp (cadr contact)))
                 (not (executable-find (car contact))))
            (format-message "`%s' isn't installed" (car contact)))))
      ;; Only a plain error (which is what `eglot-alternatives' signals)
      ;; tells us something.  If the function failed in any other way
      ;; we can't tell, and it's Eglot's job to deal with that.
      (error (and (eq (car err) 'error)
                  (error-message-string err))))))

(defun prelude-lsp--eglot-ensure (&optional quiet)
  "Start Eglot in the current buffer, unless its server isn't installed.
Eglot would complain about that for every file you open, while we
mention it just once for a major mode, and not at all if QUIET is
non-nil."
  (when buffer-file-name
    ;; looking for programs on a remote host is slow, so that's left to
    ;; Eglot, which does it only once you start working with the buffer
    (let ((missing (unless (file-remote-p default-directory)
                     (prelude-lsp--eglot-missing-server))))
      (cond
       ((not missing) (eglot-ensure))
       ((not (or quiet (memq major-mode prelude-lsp--reported-modes)))
        (push major-mode prelude-lsp--reported-modes)
        (message "[Prelude] No language server for `%s' buffers: %s"
                 major-mode missing))))))

(defun prelude-lsp-enable (&optional quiet)
  "Enable the LSP client configured via `prelude-lsp-client'.
With Eglot nothing happens when the language server of the buffer isn't
installed, apart from a message the first time that's noticed.

If QUIET is non-nil a missing language server isn't mentioned at all.
That's for the kinds of files that people edit all the time without
having any use for a language server (configuration files, shell
scripts, etc)."
  (pcase prelude-lsp-client
    ('eglot (prelude-lsp--eglot-ensure quiet))
    ('lsp-mode
     (require 'prelude-lsp-mode)
     (when quiet
       (setq-local lsp-warn-no-matched-clients nil
                   lsp-enable-suggest-server-download nil))
     (lsp-deferred))))

(defun prelude-format-on-save-p ()
  "Return non-nil if a module should format the buffer when it's saved.
That's not the case when `prelude-format-on-save' is off, or when
Apheleia is already taking care of the formatting."
  (and prelude-format-on-save
       (not (bound-and-true-p apheleia-mode))))

(defun prelude-lsp-format-on-save ()
  "Format the buffer with its language server, if there's one running.
Meant for `before-save-hook'.  See `prelude-format-on-save-p'."
  (when (prelude-format-on-save-p)
    ;; a failed request shouldn't get in the way of saving the file
    (with-demoted-errors "[Prelude] Couldn't format the buffer: %S"
      (cond
       ((and (fboundp 'eglot-managed-p) (eglot-managed-p))
        (eglot-format-buffer))
       ((bound-and-true-p lsp-mode)
        (lsp-format-buffer))))))

;; Eglot configuration
(defvar eglot-autoshutdown)
(defvar eglot-events-buffer-config)
(defvar eglot-extend-to-xref)
(defvar eglot-mode-map)
(declare-function eglot-code-action-organize-imports "eglot")
(declare-function eglot-code-actions "eglot")
(declare-function eglot-format-buffer "eglot")
(declare-function eglot-managed-p "eglot")
(declare-function lsp-format-buffer "lsp-mode")
(declare-function eglot-rename "eglot")

(with-eval-after-load 'eglot
  (setq eglot-autoshutdown t)
  ;; don't log every LSP event - the logging adds overhead with chatty
  ;; servers (set these back when you need to debug an LSP session)
  (if (boundp 'eglot-events-buffer-config)
      (setq eglot-events-buffer-config '(:size 0 :format full)) ; newer Eglot
    (with-suppressed-warnings ((obsolete eglot-events-buffer-size)
                               (free-vars eglot-events-buffer-size))
      (setq eglot-events-buffer-size 0)))                       ; older Eglot
  (setq eglot-extend-to-xref t)

  (define-key eglot-mode-map (kbd "C-c C-l r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c C-l e") #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c C-l f") #'eglot-format-buffer)
  (define-key eglot-mode-map (kbd "C-c C-l o") #'eglot-code-action-organize-imports))

(provide 'prelude-core)
;;; prelude-core.el ends here
