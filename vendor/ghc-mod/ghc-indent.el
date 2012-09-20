;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-indent.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Feb 28, 2012

;;; Code:

(defvar ghc-indent-offset 4)

(defun ghc-make-indent-shallower (beg end)
  (interactive "r")
  (let ((n ghc-indent-offset))
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(goto-char beg)
	(while (not (eobp))
	  (delete-region (point) (+ (point) n))
	  (forward-line))))))

(defun ghc-make-indent-deeper (beg end)
  (interactive "r")
  (let ((indent (make-string ghc-indent-offset 32)))
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(goto-char beg)
	(while (not (eobp))
	  (insert indent)
	  (forward-line))))))

(provide 'ghc-indent)
