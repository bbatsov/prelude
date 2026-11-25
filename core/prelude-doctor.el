;;; prelude-doctor.el --- Prelude's check of your setup. -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; M-x prelude-doctor lists what Prelude needs from your system that's
;; missing (programs, tree-sitter grammars, language servers), the
;; modules that you've enabled, the modules that were added to Prelude
;; since you made your copy of the modules file, and the obsolete
;; Prelude hooks your config still uses.  The report doubles as the
;; information a bug report needs.

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

(require 'seq)
(require 'subr-x)

(defvar prelude-dir)
(defvar prelude-modules-dir)
(defvar prelude-modules-file)
(defvar prelude-treesit--languages)
(defvar prelude-conflicting-modules)
(defvar prelude-lsp-client)
(defvar prelude-flyspell)
(defvar prelude-spell-checker)
(defvar ispell-program-name)
(declare-function prelude-lsp--eglot-missing-server "prelude-core")
(declare-function prelude-treesit--supported-p "prelude-treesit")
(declare-function treesit-language-available-p "treesit.c")

(defconst prelude-doctor-buffer-name "*Prelude Doctor*"
  "The name of the buffer that `prelude-doctor' reports in.")

(defun prelude-doctor--version ()
  "Return the version of Prelude, as git describes it, or nil."
  (let ((default-directory prelude-dir))
    (when (and (file-exists-p (expand-file-name ".git" prelude-dir))
               (executable-find "git"))
      (with-temp-buffer
        ;; what git says on stderr (e.g. about a broken repository) is
        ;; no version
        (when (zerop (call-process "git" nil '(t nil) nil
                                   "describe" "--tags" "--always" "--dirty"))
          (string-trim (buffer-string)))))))

(defun prelude-doctor--enabled-modules ()
  "Return the names of the Prelude modules that are loaded, in load order."
  (seq-filter (lambda (name)
                (file-exists-p (expand-file-name (concat name ".el")
                                                 prelude-modules-dir)))
              (mapcar #'symbol-name (reverse features))))

(defun prelude-doctor--mentioned-modules (file)
  "Return the names of the modules required in FILE, commented out or not."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((names nil))
      (while (re-search-forward "(require '\\(prelude-[a-z0-9-]+\\))" nil t)
        (push (match-string 1) names))
      (nreverse names))))

(defun prelude-doctor--new-modules ()
  "Return the modules of the sample modules file missing from yours.
Those were most likely added to Prelude after you made your copy."
  (when (file-exists-p prelude-modules-file)
    (seq-difference
     (prelude-doctor--mentioned-modules
      (expand-file-name "sample/prelude-modules.el" prelude-dir))
     (prelude-doctor--mentioned-modules prelude-modules-file))))

(defun prelude-doctor--obsolete-variables ()
  "Return the obsolete Prelude variables that your config gives a value.
Those are mostly the hooks Prelude used to run in the buffers of the
major modes, which the hooks of the modes themselves have replaced."
  (let ((found nil))
    (mapatoms
     (lambda (symbol)
       (when (and (string-prefix-p "prelude-" (symbol-name symbol))
                  (get symbol 'byte-obsolete-variable)
                  ;; an alias has the value of the variable it stands for
                  (eq (indirect-variable symbol) symbol)
                  (default-boundp symbol)
                  (default-value symbol))
         (push symbol found))))
    (sort found #'string<)))

(defun prelude-doctor--language-server (ts-mode)
  "Return what's missing for the language server of TS-MODE, or nil."
  (with-temp-buffer
    ;; enough for `derived-mode-p', without running the mode
    (setq major-mode ts-mode)
    (prelude-lsp--eglot-missing-server)))

(defun prelude-doctor--checks ()
  "Return the results of the checks as a list of (SECTION . RESULTS).
A result is (STATUS TEXT), where STATUS is `ok', `problem' or `note'."
  (let ((treesit (prelude-treesit--supported-p)))
    (list
     (cons (format "Emacs %s (%s)" emacs-version system-configuration)
           (list
            (list (if treesit 'ok 'problem)
                  (if treesit
                      "tree-sitter support"
                    "no tree-sitter support, so no tree-sitter modes"))
            (if (and (fboundp 'native-comp-available-p) (native-comp-available-p))
                (list 'ok "native compilation")
              (list 'note "no native compilation, so Emacs Lisp runs a bit slower"))))
     (cons (format "Prelude %s in %s"
                   (or (prelude-doctor--version) "(version unknown)")
                   (abbreviate-file-name prelude-dir))
           (append
            (if (file-exists-p prelude-modules-file)
                (list (list 'ok (format "modules file %s"
                                        (abbreviate-file-name prelude-modules-file))))
              (list (list 'note (format "no %s, using the sample modules file"
                                        (abbreviate-file-name prelude-modules-file)))))
            (list (list 'note (format "LSP client: %s" prelude-lsp-client)))
            (list (list 'note (format "enabled modules: %s"
                                      (string-join (prelude-doctor--enabled-modules) " "))))
            (mapcar (lambda (group)
                      (list 'problem (format "these modules do the same job, keep only one: %s"
                                             (mapconcat #'symbol-name group " "))))
                    (seq-filter #'cdr
                                (mapcar (lambda (group) (seq-filter #'featurep group))
                                        prelude-conflicting-modules)))
            (let ((new (prelude-doctor--new-modules)))
              (when new
                (list (list 'note (format "modules added since you made your copy of the modules file: %s"
                                          (string-join new " "))))))
            ;; they still work, but not for much longer
            (mapcar (lambda (variable)
                      (let ((instead (car (get variable 'byte-obsolete-variable))))
                        (list 'note (format "%s is obsolete, %s" variable
                                            (if (stringp instead)
                                                (substitute-quotes instead)
                                              (format-message "use `%s' instead." instead))))))
                    (prelude-doctor--obsolete-variables))))
     (cons "Programs"
           (delq nil
                 (list
                  (if (executable-find "git")
                      (list 'ok "git")
                    (list 'problem "git isn't installed: Magit, updating Prelude and installing grammars need it"))
                  (if (executable-find "rg")
                      (list 'ok "rg (ripgrep)")
                    (list 'note "rg (ripgrep) isn't installed: searching projects is faster with it"))
                  (when (and prelude-flyspell (eq prelude-spell-checker 'flyspell))
                    (require 'ispell)
                    (if (executable-find ispell-program-name)
                        (list 'ok ispell-program-name)
                      (list 'problem (format "%s isn't installed, so spell checking is off"
                                             ispell-program-name))))
                  (when treesit
                    (if (seq-some #'executable-find '("cc" "gcc" "c99" "clang"))
                        (list 'ok "a C compiler, for installing tree-sitter grammars")
                      (list 'problem "no C compiler, so tree-sitter grammars can't be installed"))))))
     (cons "Languages"
           (let ((results nil))
             (pcase-dolist (`(,lang . ,ts-mode)
                            (sort (seq-uniq (copy-sequence prelude-treesit--languages))
                                  (lambda (a b) (string< (car a) (car b)))))
               (when treesit
                 (push (if (treesit-language-available-p lang)
                           (list 'ok (format "%s: tree-sitter grammar" lang))
                         (list 'problem (format "%s: no tree-sitter grammar (M-x prelude-treesit-install-grammars)"
                                                lang)))
                       results))
               ;; editing works without a language server, so a missing
               ;; one isn't a problem
               (when (eq prelude-lsp-client 'eglot)
                 (let ((missing (prelude-doctor--language-server ts-mode)))
                   (push (if missing
                             (list 'note (format "%s: no language server (%s)" lang missing))
                           (list 'ok (format "%s: language server" lang)))
                         results))))
             (nreverse results))))))

(defun prelude-doctor--insert (checks)
  "Insert the report of CHECKS, see `prelude-doctor--checks'."
  (insert (propertize "Prelude Doctor" 'face 'bold) "\n\n"
          "The things marked as problems keep a part of Prelude from working.\n"
          "This report is also what a bug report needs, so paste it there.\n")
  (pcase-dolist (`(,section . ,results) checks)
    (insert "\n" (propertize section 'face 'bold) "\n")
    (pcase-dolist (`(,status ,text) results)
      (insert (pcase status
                ('ok (propertize "  ok       " 'face 'success))
                ('problem (propertize "  problem  " 'face 'error))
                (_ (propertize "  note     " 'face 'shadow)))
              text "\n"))))

(defun prelude-doctor ()
  "Check what Prelude needs from your system, and report it in a buffer.
Press \\`g' in the buffer to check again."
  (interactive)
  (with-current-buffer (get-buffer-create prelude-doctor-buffer-name)
    ;; Check this machine, even when called from a remote buffer.
    ;; (Looking for programs over TRAMP is slow, and beside the point.)
    (setq default-directory prelude-dir)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (prelude-doctor--insert (prelude-doctor--checks))
      (goto-char (point-min)))
    (special-mode)
    (setq-local revert-buffer-function (lambda (&rest _) (prelude-doctor)))
    (pop-to-buffer (current-buffer))))

(provide 'prelude-doctor)
;;; prelude-doctor.el ends here
