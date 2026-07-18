;;; prelude-welcome.el --- Prelude's welcome screen. -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.1.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A simple welcome screen shown on startup (unless Emacs was launched
;; with file arguments).  It points newcomers at the things they most
;; need - where their personal config lives, how to enable modules,
;; the user manual - and offers quick access to recent files, known
;; projects and bookmarks.  Disable it with `prelude-welcome-screen'.

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

(defvar prelude-tips)
(defvar prelude-user)
(defvar prelude-dir)
(defvar prelude-personal-dir)
(defvar prelude-modules-file)

(defconst prelude-welcome-banner
  "\
   _____  _____  ______ _     _    _ _____  ______
  |  __ \\|  __ \\|  ____| |   | |  | |  __ \\|  ____|
  | |__) | |__) | |__  | |   | |  | | |  | | |__
  |  ___/|  _  /|  __| | |   | |  | | |  | |  __|
  | |    | | \\ \\| |____| |___| |__| | |__| | |____
  |_|    |_|  \\_\\______|______\\____/|_____/|______|"
  "ASCII banner shown at the top of the Prelude welcome screen.")

(defconst prelude-welcome-buffer-name "*prelude-welcome*"
  "Name of the buffer holding the Prelude welcome screen.")

(defvar prelude-welcome-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)
    (define-key map (kbd "g") #'prelude-welcome-refresh)
    map)
  "Keymap for `prelude-welcome-mode'.")

(define-derived-mode prelude-welcome-mode special-mode "Prelude Welcome"
  "Major mode for the Prelude welcome screen."
  (setq-local cursor-type nil)
  (setq-local truncate-lines t))

(defun prelude-welcome--heading (text)
  "Insert a section heading TEXT."
  (insert "\n  " (propertize text 'face 'bold) "\n"))

(defun prelude-welcome--button (label help action)
  "Insert a button labelled LABEL with HELP that runs ACTION when pushed."
  (insert-text-button label
                      'action (lambda (_button) (funcall action))
                      'help-echo help
                      'follow-link t))

(defun prelude-welcome--modules-file ()
  "Return the personal modules file, or the bundled sample if it's absent."
  (if (and (boundp 'prelude-modules-file) (file-exists-p prelude-modules-file))
      prelude-modules-file
    (expand-file-name "sample/prelude-modules.el" prelude-dir)))

(defun prelude-welcome--insert-getting-started ()
  "Insert the getting-started links."
  (prelude-welcome--heading "Getting started")
  (insert "  ")
  (prelude-welcome--button "[Personal config]"
                           "Open your personal configuration directory"
                           (lambda () (dired prelude-personal-dir)))
  (insert "  ")
  (prelude-welcome--button "[Enable modules]"
                           "Edit the list of enabled Prelude modules"
                           (lambda () (find-file (prelude-welcome--modules-file))))
  (insert "  ")
  (prelude-welcome--button "[User manual]"
                           "Open the Prelude manual in your browser"
                           (lambda () (browse-url "https://prelude.emacsredux.com")))
  (insert "\n  ")
  (prelude-welcome--button "[Update Prelude]" "Run prelude-update"
                           #'prelude-update)
  (insert "  ")
  (prelude-welcome--button "[Update packages]" "Run prelude-update-packages"
                           #'prelude-update-packages)
  (insert "\n"))

(defun prelude-welcome--insert-recent-files (n)
  "Insert up to N recent files."
  (when (bound-and-true-p recentf-list)
    (prelude-welcome--heading "Recent files")
    (dolist (file (seq-take recentf-list n))
      (insert "  ")
      (prelude-welcome--button (abbreviate-file-name file)
                               (format "Open %s" file)
                               (lambda () (find-file file)))
      (insert "\n"))))

(defun prelude-welcome--insert-projects (n)
  "Insert up to N known projects."
  (when (bound-and-true-p projectile-known-projects)
    (prelude-welcome--heading "Projects")
    (dolist (project (seq-take projectile-known-projects n))
      (insert "  ")
      (prelude-welcome--button (abbreviate-file-name project)
                               (format "Switch to project %s" project)
                               (lambda () (projectile-switch-project-by-name project)))
      (insert "\n"))))

(defun prelude-welcome--insert-bookmarks (n)
  "Insert up to N bookmarks."
  (require 'bookmark)
  (let ((names (bookmark-all-names)))
    (when names
      (prelude-welcome--heading "Bookmarks")
      (dolist (bookmark (seq-take names n))
        (insert "  ")
        (prelude-welcome--button bookmark
                                 (format "Jump to bookmark %s" bookmark)
                                 (lambda () (bookmark-jump bookmark)))
        (insert "\n")))))

(defun prelude-welcome--insert-tip ()
  "Insert a random Prelude tip, if any are defined."
  (when (bound-and-true-p prelude-tips)
    (prelude-welcome--heading "Tip of the day")
    (insert "  " (nth (random (length prelude-tips)) prelude-tips) "\n")))

(defun prelude-welcome--render ()
  "Build (or rebuild) and return the Prelude welcome buffer."
  (let ((buf (get-buffer-create prelude-welcome-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "\n" prelude-welcome-banner "\n\n")
        (insert (propertize
                 (format "  Welcome to Prelude, Master %s!\n"
                         (or (bound-and-true-p prelude-user) "Emacs hacker"))
                 'face 'bold))
        (insert "  A sleek, modern, simple and powerful Emacs experience.\n")
        (prelude-welcome--insert-getting-started)
        (prelude-welcome--insert-recent-files 5)
        (prelude-welcome--insert-projects 5)
        (prelude-welcome--insert-bookmarks 5)
        (prelude-welcome--insert-tip)
        (insert (propertize
                 "\n  TAB/S-TAB move between links   g refresh   q bury\n"
                 'face 'font-lock-comment-face)))
      (prelude-welcome-mode)
      (goto-char (point-min)))
    buf))

(defun prelude-welcome-refresh ()
  "Rebuild the Prelude welcome screen in place."
  (interactive)
  (prelude-welcome--render))

(defun prelude-welcome ()
  "Show the Prelude welcome screen."
  (interactive)
  (switch-to-buffer (prelude-welcome--render)))

(defun prelude-welcome--file-args-p ()
  "Non-nil if Emacs was launched with existing-file arguments."
  (seq-some (lambda (arg)
              (and (not (string-prefix-p "-" arg))
                   (file-exists-p arg)))
            (cdr command-line-args)))

;; Show the welcome screen on startup, unless it's been disabled or
;; Emacs was launched to open specific files.
(when (and prelude-welcome-screen
           (not (prelude-welcome--file-args-p)))
  (setq initial-buffer-choice #'prelude-welcome--render))

(provide 'prelude-welcome)
;;; prelude-welcome.el ends here
