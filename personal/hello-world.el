;; This buffer is for text that is not saved, and for Lisp evaluation.
;;; Hello World exercise from exercism
;;; Commentary: save this exercise under $EMACSDIR/personal so this will be included
;;; in emacs path on startup
;;; Code:

(defun hello ()
  "Hello, World!"
  (interactive)
  (message "Hello, World!"))

(hello)
(provide 'hello-world)
;;; hello-world.el ends here
