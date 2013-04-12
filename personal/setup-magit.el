;;; setup-magit.el --- My personal magit configuration
;;
;; Taken from http://whattheemacsd.com/
;;
;;; Commentary:
;; Convenience defuns that make it easier to work with Magit
;;

;;; Code:

(require 'magit)

(defadvice magit-status (around magit-fullscreen activate)
  "Activate full screen when using Magit."
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restore the previous window configuration and kill the magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(defun magit-toggle-whitespace ()
  "Toggle the display of whitespace in magit diffs."
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  "Turn off the display of whitespace in magit diffs."
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  "Turn on the display of whitespace in magit diffs."
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

;;; setup-magit.el ends here
