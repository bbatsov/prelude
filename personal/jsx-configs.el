;;; JSX Flycheck syntax checker -- h/t

;;; Code:
(eval-after-load "flycheck"
  '(progn
     (flycheck-define-checker jsxhint-checker
      "A JSX syntax and style checker based on JSXHint."

      :command ("jsxhint" source)
      :error-patterns
      ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
      :modes (web-mode))))
(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck
              (flycheck-select-checker 'jsxhint-checker)
              (flycheck-mode))))

(provide 'jsx-configs)

;;; jsx-configs.el ends here
