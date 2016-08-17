;;; python-configs.el -- configs to extend prelude's python support

;;; Commentary:
;; This package provides configuration values to augment prelude's
;; python support.

;;; Code:
(prelude-require-package 'pyvenv)
(require 'anaconda-mode)
(require 'pyvenv)

(add-hook 'pyvenv-post-activate-hooks 'pyvenv-restart-python)
(add-hook 'python-mode-hook 'pyenv-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(provide 'python-configs)
;;; python-configs.el ends here
