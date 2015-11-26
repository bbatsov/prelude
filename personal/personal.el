;;; personal.el -- root package for jgrillo's prelude extensions

;;; Commentary:
;; This package is the root package for jgrillo's personal extensions
;; of prelude. It is responsible for setting custom config values.

;;; Code:
(setq visible-bell t
      company-idle-delay 0.2
      company-minimum-prefix-length 1
      company-tooltip-align-annotations t)

(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

(provide 'personal)
;;; personal.el ends here
