;;; prelude-ui.el --- Emacs Prelude: UI optimizations and tweaks.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; We dispense with most of the point and click UI, reduce the startup noise,
;; configure smooth scolling and a nice theme that's easy on the eyes (Tokyo Night).

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

;; the toolbar is disabled via `default-frame-alist' in early-init.el,
;; so the initial frame is never drawn with one

(when prelude-minimalistic-ui
  (menu-bar-mode -1))

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; enable smooth pixel scrolling on graphical displays
(pixel-scroll-precision-mode t)

;; a right click brings up a menu for the thing that was clicked,
;; instead of extending the region
(context-menu-mode +1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; show line numbers at the beginning of each line
(unless prelude-minimalistic-ui
  (global-display-line-numbers-mode))

;; enable y/n answers
(setq use-short-answers t)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " Prelude - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

;; Tokyo Night is the default theme: its light variant during the day,
;; the dark one at night
(use-package tokyo-night
  :ensure t
  :defer t)

;; Zenburn was the default for many years, and plenty of configs still
;; ask for it.
(when (memq 'zenburn (list prelude-theme (car-safe prelude-theme) (cdr-safe prelude-theme)))
  (use-package zenburn-theme
    :ensure t
    :defer t))

(defconst prelude-theme--default-daytime '(7.0 . 19.0)
  "The hours of the day to fall back to when there's nothing better.")

;; defined in solar.el, which is loaded only if it's needed
(defvar calendar-latitude)
(defvar calendar-longitude)
(declare-function calendar-current-date "calendar")
(declare-function solar-sunrise-sunset "solar")

(defvar prelude-theme--sun nil
  "The last result of `prelude-theme--sun-hours', as (KEY . HOURS).
KEY is the date and the location that it was computed for.")

(defun prelude-theme--hours (time)
  "Convert TIME, a string like \"07:30\", to a number of hours."
  (let ((parts (mapcar #'string-to-number (split-string time ":"))))
    (+ (car parts) (/ (or (cadr parts) 0) 60.0))))

(defun prelude-theme--sun-hours ()
  "Return today's sunrise and sunset as a cons of hours.
That's `prelude-theme--default-daytime' if Emacs doesn't know where it
is, or if the sun doesn't rise or set there today."
  (require 'solar)
  (let* ((date (calendar-current-date))
         (key (list date calendar-latitude calendar-longitude)))
    (unless (equal (car prelude-theme--sun) key)
      (let ((sun (and calendar-latitude calendar-longitude
                      (solar-sunrise-sunset date))))
        (setq prelude-theme--sun
              (cons key
                    (if (and (car sun) (cadr sun))
                        (cons (car (car sun)) (car (cadr sun)))
                      prelude-theme--default-daytime)))))
    (cdr prelude-theme--sun)))

(defun prelude-theme--daytime ()
  "Return the hours between which it's day, as a cons (START . END)."
  (pcase prelude-theme-daytime
    (`(,(and start (pred stringp)) . ,(and end (pred stringp)))
     (cons (prelude-theme--hours start) (prelude-theme--hours end)))
    ('sun (prelude-theme--sun-hours))
    (_ prelude-theme--default-daytime)))

;; set by Emacs Plus (macOS) and by Emacs 31 on some platforms
(defvar ns-system-appearance)
(defvar toolkit-theme)

(defun prelude-theme--system-appearance ()
  "Return the appearance of the system, `light' or `dark'.
That's nil if this Emacs isn't told about it."
  (car (memq (or (bound-and-true-p ns-system-appearance)
                 (bound-and-true-p toolkit-theme))
             '(light dark))))

(defun prelude-theme-for-time (&optional time)
  "Return the theme that `prelude-theme' calls for at TIME.
TIME defaults to the current time.  It doesn't matter if
`prelude-theme-daytime' is `system' and the system's appearance is
known."
  (if (consp prelude-theme)
      (pcase (and (eq prelude-theme-daytime 'system)
                  (prelude-theme--system-appearance))
        ('light (car prelude-theme))
        ('dark (cdr prelude-theme))
        (_ (prelude-theme--for-clock time)))
    prelude-theme))

(defun prelude-theme--for-clock (time)
  "Return the day or the night theme of `prelude-theme' for TIME."
  (let* ((now (decode-time time))
         (hour (+ (decoded-time-hour now)
                  (/ (decoded-time-minute now) 60.0)))
         (day (prelude-theme--daytime))
         (after-start (<= (car day) hour))
         (before-end (< hour (cdr day))))
    ;; a day that starts in the evening ends on the next morning
    (if (if (<= (car day) (cdr day))
            (and after-start before-end)
          (or after-start before-end))
        (car prelude-theme)
      (cdr prelude-theme))))

(defvar prelude-theme--current nil
  "The theme that Prelude loaded last.")

(defvar prelude-theme--broken nil
  "The themes that Prelude has failed to load.")

(defun prelude-theme-sync ()
  "Load the theme that `prelude-theme' calls for at this time of day.
Nothing happens if that theme is in place already, or if you've picked
another theme yourself in the meantime (e.g. with `load-theme')."
  (let ((theme (prelude-theme-for-time))
        (old prelude-theme--current))
    (when (and theme
               (not (eq theme old))
               (not (memq theme prelude-theme--broken))
               (or (null old) (eq (car custom-enabled-themes) old)))
      (condition-case err
          (progn
            (load-theme theme t)
            (setq prelude-theme--current theme)
            (when old
              (disable-theme old)))
        ;; most likely a theme that isn't installed; say so once and
        ;; keep the theme that's in place
        (error
         (push theme prelude-theme--broken)
         (display-warning
          'prelude
          (format "Couldn't load the theme `%s' (see `prelude-theme'): %s"
                  theme (error-message-string err))))))))

(prelude-theme-sync)

(defvar prelude-theme--timer nil
  "The timer that keeps the theme in sync with the time of day.")

(defun prelude-theme--system-changed (&rest _)
  "Follow the system when it switches between light and dark mode."
  (prelude-theme-sync))

;; a day and a night theme: keep checking which one it's time for, and
;; listen to the system where it tells Emacs about its appearance
(when (timerp prelude-theme--timer)
  (cancel-timer prelude-theme--timer))
(when (consp prelude-theme)
  (setq prelude-theme--timer (run-at-time 60 60 #'prelude-theme-sync))
  (add-hook 'ns-system-appearance-change-functions #'prelude-theme--system-changed)
  (add-hook 'toolkit-theme-set-functions #'prelude-theme--system-changed))


;; built into Emacs 30+
(use-package which-key
  :ensure t
  :defer t)

;; NOTE(@lerax): dom 01 jun 2025 12:42:24
;; helm-descbinds became incompatible with which-key-mode ins 202402XX version
;; Using prelude, calling which-key-mode as hook in
;; server-after-make-frame-hook crash terminal daemoned session
;; this function prevents to this happen
(defun prelude-safe-which-key-mode ()
  (condition-case err
      (which-key-mode +1)
    (error
     (let ((error-message (cadr err)))
       (with-temp-message "" ;; don't print to minibuffer
         (message "[Prelude] bypass error: %s" error-message))))))

;; show available keybindings after you start typing
;; add to hook when running as a daemon as a workaround for a
;; which-key bug
;; https://github.com/justbur/emacs-which-key/issues/306
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'prelude-safe-which-key-mode)
  (prelude-safe-which-key-mode))

(provide 'prelude-ui)
;;; prelude-ui.el ends here
