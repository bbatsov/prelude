;;; custom-fns.el --- My custom this-and-that
;;
;;; Commentary:
;; Functions that make my life easier.
;;; Code:

;;-----------------A Handy-Dandy function for Rotating Windows------------------
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)

;;------------------------------------Today-------------------------------------
(defun insert-iso-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d" (current-time))))

(global-set-key (kbd "C-x j") 'insert-iso-date)

;;; Tweak commenting behavior
(defun comment-dwim-line (&optional arg)
  "Replacement for the `comment-dwim' command.

If no region is selected and current line is not blank
        and we are not at the end of the line, then comment
        current line.  Replaces default behaviour of
        `comment-dwim', when it inserts comment at the end of the
        line.  With an argument, passes ARG to `comment-dwim'"
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(global-set-key (kbd "M-;") `comment-dwim-line)

;;-----------------------Marked.app Support for Markdown------------------------
;; Assumes you're on a Mac, and have Marked.app installed

(defun markdown-preview-file ()
  "Run Marked on the current file and revert the buffer."
  (interactive)
  (shell-command
   (format "open -a /Applications/Marked.app %s"
           (shell-quote-argument (buffer-file-name)))))
(global-set-key (kbd "C-c m") 'markdown-preview-file)

(provide 'custom-fns)

;;; custom-fns.el ends here
