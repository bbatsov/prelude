;;; pass-store.el -- Emacs library for working with pass.

;; Copyright (C) 2016 Jesse C. Grillo <jesse.grillo@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This work was inspired by Svend Sorenson's password-store.el:
;;
;; https://git.zx2c4.com/password-store/tree/contrib/emacs/password-store.el
;;
;; The reasons for this rewrite were a desire to seperate UI concerns
;; from the storage API, to bypass the kill ring in favor of the
;; system clipboard, and to implement some functionality which is
;; missing from password-store.el.

;;; Usage:
;;
;; This package provides a library of functions constituting an Elisp
;; API and an Emacs user interface for working with the pass program:
;;
;; https://www.passwordstore.org/
;;
;; Installing pass in a path-available location is a prerequisite for
;; using this library. You can likely install it using your system's
;; package manager (or homebrew if you are one of those people).
;;
;; To use this library, you should require it in your Emacs
;; configuration (init.el or similar):
;;
;; (load-library "pass-store")
;; (require 'pass-store)
;; ;;; optionally override configuration defaults here, i.e.
;; ;; (setq 'pass-store-password-length 666)
;; (pass-store-configure)
;;
;; In the default configuration, all pass-store commands begin with
;; the prefix "C-c C-p". Search commands are prefixed with "C-c C-p s"
;; and (most) commands which modify the password store are prefixed
;; with "C-c C-p m". Commands which are prefixed by only "C-c C-p"
;; are, for lack of better words, referred to here as "root commands".
;;
;; Root commands:
;;
;;   "C-c C-p p" -- Decrypt and copy the password having a
;;   user-supplied name to the clipboard. The password will remain in
;;   the clipboard for 45 seconds.
;;
;;   "C-c C-p i" -- Initialize the password store for a user-supplied
;;   GPG key ID.
;;
;;   "C-c C-p g" -- Safely run a user-supplied git command on the
;;   password store.
;;
;; Search commands:
;;
;;   "C-c C-p s l" -- List all password names in the minibuffer (using
;;   ido-mode). Selecting a password name decrypts and copies the
;;   underlying password to the clipboard, where it will remain for 45
;;   seconds.
;;
;;   "C-c C-p s g" -- List all password names in the minibuffer (using
;;   ido-mode) whose name or encrypted data matches a user-supplied
;;   search string. Selecting a password name decrypts and copies the
;;   underlying password to the clipboard, where it will remain for 45
;;   seconds.
;;
;;   "C-c C-p s f" -- List all password names in the minibuffer (using
;;   ido-mode) whose name matches any of the user supplied name
;;   strings. Selecting a password name decrypts and copies the
;;   underlying password to the clipboard, where it will remain for 45
;;   seconds.
;;
;; Modification commands:
;;
;;   "C-c C-p m i" -- Insert and encrypt a password record into the
;;   password store. Prompts the user for all required input.
;;
;;   "C-c C-p m g" -- Generate a random password and insert it into the
;;   password store under a user-supplied name.
;;
;;   "C-c C-p m r" -- Remove a password having the user-supplied name
;;   from the password store.
;;
;;   "C-c C-p m m" -- Move a password from one user-supplied location
;;   in the password store to another location in the password store.
;;
;;   "C-c C-p m c" -- Copy a password from one user-supplied location
;;   in the password store to another location in the password store.

;;; Code:
(require 's)
(require 'simpleclip)
(require 'ido)
(require 'ert)

(defgroup pass-store '()
  "Emacs mode for dealing with passwords."
  :prefix "pass-store-"
  :group 'pass-store)

(defcustom pass-store-password-length 48
  "Default password length."
  :group 'pass-store
  :type 'integer)

(defcustom pass-store-password-symbols t
  "Whether to generate passwords containing symbols."
  :group 'pass-store
  :type 'boolean)

(defcustom pass-store-init-cmd-key "C-c C-p i"
  "Key combo for pass-store-init-cmd."
  :group 'pass-store
  :type 'string)

(defcustom pass-store-ls-cmd-key "C-c C-p s l"
  "Key combo for pass-store-ls-cmd."
  :group 'pass-store
  :type 'string)

(defcustom pass-store-grep-cmd-key "C-c C-p s g"
  "Key combo for pass-store-grep-cmd."
  :group 'pass-store
  :type 'string)

(defcustom pass-store-find-cmd-key "C-c C-p s f"
  "Key combo for pass-store-find-cmd."
  :group 'pass-store
  :type 'string)

(defcustom pass-store-show-cmd-key "C-c C-p p"
  "Key combo for pass-store-show-cmd."
  :group 'pass-store
  :type 'string)

(defcustom pass-store-insert-cmd-key "C-c C-p m i"
  "Key combo for pass-store-insert-cmd."
  :group 'pass-store
  :type 'string)

(defcustom pass-store-generate-cmd-key "C-c C-p m g"
  "Key combo for pass-store-generate-cmd."
  :group 'pass-store
  :type 'string)

(defcustom pass-store-rm-cmd-key "C-c C-p m r"
  "Key combo for pass-store-rm-cmd."
  :group 'pass-store
  :type 'string)

(defcustom pass-store-mv-cmd-key "C-c C-p m m"
  "Key combo for pass-store-mv-cmd."
  :group 'pass-store
  :type 'string)

(defcustom pass-store-cp-cmd-key "C-c C-p m c"
  "Key combo for pass-store-cp-cmd."
  :group 'pass-store
  :type 'string)

(defcustom pass-store-git-cmd-key "C-c C-p g"
  "Key combo for pass-store-git-cmd."
  :group 'pass-store
  :type 'string)

(defvar pass-store-pass-executable (executable-find "pass")
  "The pass utility executable.")

;;
;; "private" helper functions
;;
;; TODO: make a better run function which properly communicates with
;; the running process using process-send-string, process-send-region,
;; process-send-eof, etc. See:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Input-to-Processes.html
;;
;; TODO: tests
;; TODO: this should probably return a process handle-args-function-alist
;; TODO: password store updates should happen in an atomic-change-group
;; TODO: use unwind-protect to make sure things are cleaned up properly

(defun pass-store--run (command input-buffer &rest args)
  "Run PASS-STORE-PASS-EXECUTABLE with the supplied COMMAND and ARGS.

Nil ARGS are ignored. Returns"
  (with-temp-buffer
    (let* ((tempfile (make-temp-file ""))
           (exit-code
            (apply 'call-process
                   (append
                    (list pass-store-pass-executable nil (list t tempfile) nil)
                    (delq nil args)))))
      (unless (zerop exit-code)
        (erase-buffer)
        (insert-file-contents tempfile))
      (delete-file tempfile)
      (if (zerop exit-code)
          (s-chomp (buffer-string))
        (error (s-chomp (buffer-string)))))))

(ert-deftest test-pass-store--run ()
  "Test that pass-store--run "
  )

;;
;; configuration function
;;

(defun pass-store-configure ()
  "Configure Emacs to use the pass-store keybindings."
  (global-set-key (kbd pass-store-init-cmd-key) 'pass-store-init-cmd)
  (global-set-key (kbd pass-store-ls-cmd-key) 'pass-store-ls-cmd)
  (global-set-key (kbd pass-store-grep-cmd-key) 'pass-store-grep-cmd)
  (global-set-key (kbd pass-store-find-cmd-key) 'pass-store-find-cmd)
  (global-set-key (kbd pass-store-show-cmd-key) 'pass-store-show-cmd)
  (global-set-key (kbd pass-store-insert-cmd-key) 'pass-store-insert-cmd)
  (global-set-key (kbd pass-store-generate-cmd-key) 'pass-store-generate-cmd)
  (global-set-key (kbd pass-store-rm-cmd-key) 'pass-store-rm-cmd)
  (global-set-key (kbd pass-store-mv-cmd-key) 'pass-store-mv-cmd)
  (global-set-key (kbd pass-store-cp-cmd-key) 'pass-store-cp-cmd)
  (global-set-key (kbd pass-store-git-cmd-key) 'pass-store-git-cmd))

;;
;; public pass-store API
;;
;; TODO: tests

(defun pass-store-init (gpg-ids &optional subfolder)
  "Initialize password storage for the given GPG-ID.

If SUBFOLDER is present, and the password store has already been
initialized with another GPG-ID, encrypt SUBFOLDER with the
supplied GPG-ID, leaving the rest of the password store encrypted
with the other GPG-ID."
  (pass-store--run "init"
                   (if subfolder (format "--path=%s" subfolder))
                   gpg-ids))

(defun pass-store-ls (subfolder)
  "List names of passwords in the tree rooted at SUBFOLDER."
  (pass-store--run "ls" subfolder))

(defun pass-store-grep (search-string)
  "Search inside records in the password store for SEARCH-STRING."
  (pass-store--run "grep" search-string))

(defun pass-store-find (&rest pass-names)
  "List names of all passwords in the tree matching PASS-NAMES."
  (pass-store--run "find" pass-names))

(defun pass-store-show (pass-name &optional clip)
  "Return the decrypted password matching PASS-NAME.

If CLIP is non-nil, return nil and copy the decrypted password to
the clipboard, where it will remain for 45 seconds."
  (pass-store--run "show"
   (if clip "--clip")
   pass-name))

(defun pass-store-insert (pass-name &optional echo multiline force)
  "Insert a new password PASS-NAME into the password store.

If MULTILINE is true, "
  ;; TODO: accept input from stdin
  (pass-store--run "insert"
                   (if echo "--echo")
                   (if multiline "--multiline")
                   (if force "--force")
                   pass-name))

(defun pass-store-generate
    (pass-name &optional symbols clip in-place force pass-length)
  "Generate a new password PASS-NAME.

See the pass docs for the significance of SYMBOLS, CLIP,
IN-PLACE, FORCE, and PASS-LENGTH."
  (pass-store--run "generate"
                   (if (not symbols) "--no-symbols")
                   (if clip "--clip")
                   (if in-place "--in-place")
                   (if force "--force")
                   pass-name
                   (if pass-length
                       pass-length
                     pass-store-password-length)))

(defun pass-store-rm (pass-name &optional recursive force)
  "Remove the password PASS-NAME from the password store.

See the pass docs for the significance of RECURSIVE and FORCE."
  (pass-store--run "rm"
                   (if recursive "--recursive")
                   (if force "--force")
                   pass-name))

(defun pass-store-mv (old-path new-path &optional force)
  "Rename the password or directory OLD-PATH to NEW-PATH.

See the pass docs for the significance of FORCE."
  (pass-store--run "mv"
                   (if force "--force")
                   old-path
                   new-path))

(defun pass-store-cp (old-path new-path &optional force)
  "Copy the password or directory OLD-PATH to NEW-PATH.

See the pass docs for the significance of FORCE."
  (pass-store--run "cp"
                   (if force "--force")
                   old-path
                   new-path))

(defun pass-store-git (git-command-args)
  "If the password store is a git repo, feed GIT-COMMAND-ARGS to git."
  (pass-store--run "git" git-command-args))

;;
;; interactive pass-store commands
;;
;; TODO: tests

(defun pass-store-init-cmd (gpg-id &optional subfolder)
  ""
  (interactive "")
  (pass-store-init gpg-id subfolder))

(defun pass-store-ls-cmd (&optional subfolder)
  ""
  (interactive "")
  (pass-store-ls subfolder))

(defun pass-store-grep-cmd (search-string)
  ""
  (interactive "")
  (pass-store-grep search-string))

(defun pass-store-find-cmd (pass-names)
  ""
  (interactive "")
  (pass-store-find pass-names))

(defun pass-store-show-cmd (pass-name &optional clip)
  ""
  (interactive "")
  (pass-store-show pass-name clip))

(defun pass-store-insert-cmd (pass-name &optional echo multiline force)
  ""
  (interactive "")
  (pass-store-insert pass-name echo multiline force))

(defun pass-store-generate-cmd
    (pass-name &optional symbols clip in-place force pass-length)
  ""
  (interactive "")
  (pass-store-generate pass-name symbols clip in-place force pass-length))

(defun pass-store-rm-cmd (pass-name &optional recursive force)
  ""
  (interactive "")
  (pass-store-rm pass-name
                 (if recursive recursive)
                 (if force force)))

(defun pass-store-mv-cmd (old-path new-path &optional force)
  ""
  (interactive "")
  (pass-store-mv old-path new-path (if force force)))

(defun pass-store-cp-cmd (old-path new-path &optional force)
  ""
  (interactive "")
  (pass-store-cp old-path new-path (if force force)))

(defun pass-store-git-cmd (git-command-args)
  ""
  (interactive "")
  (pass-store-git git-command-args))

(provide 'pass-store)
;;; pass-store.el ends here
