;; Command + Enter to toggle whether the Emacs frame is maximized
;; Note: does nothing in terminal mode.
(global-set-key (kbd "<s-return>") 'toggle-frame-maximized)

;; Split horizontally when opening a new window from a command
;; whenever possible.
(setq split-height-threshold nil)
