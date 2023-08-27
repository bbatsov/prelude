;; number-get-prompt.el
;; Sun 27 Aug 2023 09:07:17 PM +03
;; Commentary: ask user for a number prompt
;; Code:

(defun my/get-number-prompt ()
  "Prompt user to enter a number, with input history support."
  (interactive)
  (let (n)
    ;; note the 10 is the default value if none is given
    (setq n (read-number "Please enter a number: " 10))
    (message "The number is %s" n)))

(my/get-number-prompt)
