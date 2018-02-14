
;; no menubar, no scrollbar
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq frame-title-format "")


;; don't show confusing virtual buffers that list previously opened files
(setq ivy-use-virtual-buffers nil)

;; allow arrow keys, no nagging
(setq prelude-guru nil)


;; pleasant when in a GUI
(setq-default cursor-type '(bar . 2))
(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 30))
(add-to-list 'default-frame-alist '(font . "Roboto Mono Medium 10"))
(add-to-list 'default-frame-alist '(cursor-color . "#ff69b4"))





(setq explicit-shell-file-name "C:/Program Files/Git/bin/bash.exe")
(setq shell-file-name explicit-shell-file-name)
(add-to-list 'exec-path "C:/Program Files/Git/bin")


(add-hook 'clojure-mode-hook #'smartparens-strict-mode)


(defun my-ivy-find-files ()
  "Visit all current completion candidates as files.
Switch to the buffer visiting the first candidate."
  (interactive)
  (let ((files (all-completions "" (ivy-state-collection ivy-last))))
    (mapc #'find-file-noselect (cdr files))
    (find-file (car files)))
  (minibuffer-keyboard-quit))

