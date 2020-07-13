;; Provides vi's % style bracket matching. (press % to match the
;; bracket when the context is a bracket) Much easier then the
;; C-M-f and C-M-b in emacs.
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(global-set-key "%" 'match-paren)
