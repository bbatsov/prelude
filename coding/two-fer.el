;;; two-fer.el exercise from exercism website
;;; Commentary:
;;; Sat 26 Aug 2023 12:18:19 PM +03
;;; Code:

(defun two-fer (&optional name)
  "Print 'One for you one for me' if no NAME is provided."
  (format "One for %s, one for me." (or name "you")))


;;; (two-fer)
;;; (two-fer "Alice")
(provide 'two-fer)
;;; two-fer.el ends here
