(with-eval-after-load 'web-mode
  (defun disable-highlight-long-lines ()
    (whitespace-mode nil))

  (add-hook 'web-mode-hook 'disable-highlight-long-lines))
