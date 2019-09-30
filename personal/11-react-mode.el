(prelude-require-packages '(rjsx js-doc flycheck web-mode))
(add-to-list 'auto-mode-alist '("components\\/.*\\/.js\\'" . rjsx-mode))

(require 'flycheck)
(require 'rjsx-mode)
(require 'js-doc)
(add-hook 'after-init-hook #'global-flycheck-mode)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

(flycheck-add-mode 'javascript-eslint 'web-mode)

(setq-default flycheck-temp-prefix ".flycheck")

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlint)))
(setq-default js-indent-level 2)
(setq-default js2-basic-offset 2)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default web-mode-markup-indent-offset 2)
