;;; ESS:
;; Load ESS
(require 'ess-site)

(add-to-list 'ess-style-alist
             '(my-RRR (ess-indent-level . 2)
                      (ess-first-continued-statement-offset . 2)
                      ;; (ess-first-continued-statement-offset . 0)
                      (ess-continued-statement-offset . 0)
                      ;; (ess-continued-statement-offset . 4)
                      (ess-brace-offset . 0)
                      (ess-arg-function-offset . 4)
                      (ess-arg-function-offset-new-line . '(4))
                      (ess-expression-offset . 4)
                      (ess-else-offset . 0)
                      (ess-close-brace-offset . 0)))

(setq ess-default-style 'my-RRR)
