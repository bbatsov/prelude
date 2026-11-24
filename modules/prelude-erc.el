;;; prelude-erc.el --- Emacs Prelude: ERC mode configuration.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for ERC mode, which should make your
;; IRC experience a bit more pleasant.

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

(declare-function erc-quit-server "erc")
(declare-function erc-server-or-unjoined-channel-buffer-p "erc")
(declare-function persp-switch "ext:perspective")
(declare-function persp-kill "ext:perspective")
(declare-function erc-server-buffer-p "erc")
(declare-function erc-tls "erc")
(defvar prelude-flyspell)
;; ERC's own options
(defvar erc-interpret-mirc-color)
(defvar erc-kill-buffer-on-part)
(defvar erc-kill-queries-on-quit)
(defvar erc-kill-server-buffer-on-quit)
(defvar erc-query-display)
(defvar erc-track-exclude-types)
(defvar erc-modules)
(defvar erc-server-coding-system)
(defvar erc-log-channels-directory)
(defvar erc-save-buffer-on-part)
(defvar erc-auto-discard-away)
(defvar erc-autoaway-idle-seconds)
(defvar erc-autoaway-idle-method)

(define-obsolete-variable-alias 'my-fav-irc 'prelude-erc-servers "2.2.0")

(defcustom prelude-erc-servers '("irc.libera.chat")
  "The IRC servers that `prelude-erc-start' connects to, over TLS."
  :type '(repeat string)
  :group 'prelude)

(define-obsolete-variable-alias 'bye-irc-message 'prelude-erc-quit-message "2.2.0")

(defcustom prelude-erc-quit-message "Asta la vista"
  "The message that `prelude-erc-stop' sends when it leaves a server."
  :type 'string
  :group 'prelude)

(defcustom prelude-new-irc-persp nil
  "Non-nil means that `prelude-erc-start' starts IRC in a perspective of its own.
That takes the perspective package."
  :type 'boolean
  :group 'prelude)

;; ERC's options are set now, before ERC is loaded, so that your
;; personal config (loaded after the modules) and Customize can change
;; them.  ERC's own definitions leave values that are set alone.

;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)

;; Kill the buffers of channels after /part, and the buffers of
;; queries and the server after quitting the server
(setq erc-kill-buffer-on-part t
      erc-kill-queries-on-quit t
      erc-kill-server-buffer-on-quit t)

;; open query buffers in the current window
(setq erc-query-display 'buffer)

;; exclude boring stuff from tracking
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))

;; utf-8 always and forever
(setq erc-server-coding-system '(utf-8 . utf-8))

;; where the logs go, if you turn on logging (the `log' module)
(setq erc-log-channels-directory "~/.erc/logs/"
      erc-save-buffer-on-part t)

;; ERC creates the log directory, but not the directories above it
(with-eval-after-load 'erc-log
  (when (stringp erc-log-channels-directory)
    (make-directory erc-log-channels-directory t)))

;; set your status to away after 10 minutes without input (the
;; `autoaway' module)
(setq erc-auto-discard-away t
      erc-autoaway-idle-seconds 600
      erc-autoaway-idle-method 'emacs)

;; The modules are added to ERC's own list, once there's one
(with-eval-after-load 'erc
  ;; truncate long irc buffers
  (add-to-list 'erc-modules 'truncate)
  ;; spell checking, with Flyspell
  (when prelude-flyspell
    (add-to-list 'erc-modules 'spelling)))

(defun prelude-erc--connect (server)
  "Connect securely to IRC SERVER over TLS at port 6697.
The nickname is `erc-nick', as ERC works it out."
  (erc-tls :server server
           :port 6697))

(define-obsolete-function-alias 'connect-to-erc #'prelude-erc--connect "2.2.0")

(defun prelude-erc-start ()
  "Connect to the IRC servers in `prelude-erc-servers'."
  (interactive)
  (when (y-or-n-p "Do you want to start IRC? ")
    (when (and prelude-new-irc-persp (fboundp 'persp-switch))
      (persp-switch "IRC"))
    (mapc #'prelude-erc--connect prelude-erc-servers)))

(define-obsolete-function-alias 'start-irc #'prelude-erc-start "2.2.0")

(defun prelude-erc--server-buffers ()
  "Return the buffers of the IRC servers that ERC is connected to."
  (require 'erc)
  (seq-filter (lambda (buffer)
                (if (fboundp 'erc-server-or-unjoined-channel-buffer-p)
                    (erc-server-or-unjoined-channel-buffer-p buffer) ; ERC 5.6+
                  (with-suppressed-warnings ((obsolete erc-server-buffer-p))
                    (erc-server-buffer-p buffer))))
              (buffer-list)))

(define-obsolete-function-alias 'filter-server-buffers #'prelude-erc--server-buffers "2.2.0")

(defun prelude-erc-stop ()
  "Disconnect from all the IRC servers, with `prelude-erc-quit-message'."
  (interactive)
  (when (and prelude-new-irc-persp (fboundp 'persp-switch))
    (persp-switch "IRC"))
  (dolist (buffer (prelude-erc--server-buffers))
    (message "Server buffer: %s" (buffer-name buffer))
    (with-current-buffer buffer
      (erc-quit-server prelude-erc-quit-message)))
  (when (and prelude-new-irc-persp (fboundp 'persp-kill))
    (persp-kill "IRC")))

(define-obsolete-function-alias 'stop-irc #'prelude-erc-stop "2.2.0")

(provide 'prelude-erc)
;;; prelude-erc.el ends here
