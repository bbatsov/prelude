;; menu bar - yes
(menu-bar-mode 1)

;; don't show confusing virtual buffers that list previously opened files
(setq ivy-use-virtual-buffers nil)

;; allow arrow keys, no nagging
(setq prelude-guru nil)

(if (display-graphic-p)
    (message "window system")
  (message "not a window system")
  )

;; pleasant when in a GUI
(setq-default cursor-type '(bar . 2))
(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 30))
(add-to-list 'default-frame-alist '(font . "Roboto Mono Medium 10"))
(add-to-list 'default-frame-alist '(cursor-color . "#ff69b4"))

;; cool looking mode line?
;; (require 'smart-mode-line)
;; (sml/setup)
