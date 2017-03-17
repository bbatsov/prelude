(prelude-require-packages '(company))

;; Don't throw away case inside simple text completions
;; Paul Cowan, you magnificent bastard https://twitter.com/dagda1/status/596631609048211456
(setq company-dabbrev-downcase nil)
