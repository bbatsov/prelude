;; Command + Enter to toggle whether the Emacs frame is maximized
;; Note: does nothing in terminal mode.
(global-set-key (kbd "<s-return>") 'toggle-frame-maximized)

;; Split horizontally when opening a new window from a command
;; whenever possible.
(setq split-height-threshold nil)

(defun frontside-windowing-adjust-split-width-threshold ()
  "Change the value of `split-width-threshold' so that it will cause the screen
split once and only once.

For example, if the frame is 360 columns wide, then we want the
split-width-threshold to be 181. That way, when you split horizontally, the two
new windows will each be 180 columns wide, and sit just below the threshold.
"
  (setq split-width-threshold (+ 1 (/ (frame-width) 2))))

;; recaculate split-width-threshold with every change
(add-hook 'window-configuration-change-hook
          'frontside-windowing-adjust-split-width-threshold)

;; disable window-system in terminal mode
(unless window-system
  (menu-bar-mode -1))
