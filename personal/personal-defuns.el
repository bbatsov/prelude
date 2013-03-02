;;; personal-defuns.el --- My elisp helper functions
;;
;; I might want to shift this into a minor mode when I learn how to do that.
;;
;;; Commentary:
;; Convenience defuns that I sue myself

;;; Code:

;; Shortcut C-S-return
(defun newline-previous ()
  "Insert a blank line above the cursor and move the cursor up one line."
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line)
  (indent-according-to-mode))

;; Shortcut C-return
(defun newline-next ()
  "Insert an indented newline after the current line and move the point to it."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(provide 'personal-defuns)
;;; personal-defuns.el ends here
