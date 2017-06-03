;;; rubby-configs.el -- configs to extend prelude's built-in ruby support

;;; Commentary:
;; This package provides configuration values to augment prelude's
;; ruby support.

;;; Code:
(prelude-require-packages '(projectile-rails rinari))

(add-hook 'ruby-mode-hook
          (lambda ()
            (rinari-minor-mode)
            (projectile-rails-mode)))

(provide 'rubby-configs)
;;; rubby-configs.el ends here
