;;; personal-defuns.el --- My elisp helper functions
;;
;; I might want to shift this into a minor mode when I learn how to do that.
;;
;;; Commentary:
;; Convenience defuns that I use myself

;;; Code:

;; Shortcut C-S-return
(defun newline-previous ()
  "Insert a blank line above the current line and move the point to it."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

;; Shortcut C-return
(defun newline-next ()
  "Insert an indented newline after the current line and move the point to it."
  (interactive)
  (end-of-line)
  (newline-and-indent))

;; Show line numbers only when moving by line
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively 'goto-line))
    (linum-mode -1)))

(provide 'personal-defuns)
;;; personal-defuns.el ends here
