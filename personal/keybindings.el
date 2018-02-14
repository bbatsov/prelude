

;; pop out a window to its own frame
(defun pop-out-window ()
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

(global-set-key [(control ?x) (meta ?p)] 'pop-out-window)


