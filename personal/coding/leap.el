;;; leap.el --- Leap year exercise exercism https://exercism.org/tracks/emacs-lisp/exercises/leap
;;; Commentary:
;;; Code:
;; leap year check


(defun leap-year-p (year)
  "Return whether the given YEAR is a leap-year or no."
  (when (= (mod year 4) 0)
    (if (= (mod year 100) 0)
        (when (= (mod year 400) 0) t)
      t)))

(defun ask-year ()
  "Prompt user to enter a year and check if it is a leap year."
  (interactive)
  (let (n)
    (setq n (read-number "Enter a year: "))
    (if (leap-year-p n)
        (message "%s is a leap year." n)
      (message "Nope, %s is not a leap year." n))))

(ask-year)
;;; leap.el ends here
