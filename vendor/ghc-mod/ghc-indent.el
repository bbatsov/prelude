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
  (indent-rigidly (region-beginning) (region-end) (- ghc-indent-offset)))

(defun ghc-make-indent-deeper (beg end)
  (interactive "r")
  (indent-rigidly (region-beginning) (region-end) ghc-indent-offset))

(provide 'ghc-indent)
