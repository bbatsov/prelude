(require 'prelude-programming)

(prelude-require-packages '(fsharp-mode eglot-fsharp))

(add-to-list 'auto-mode-alist '("\\.fs\\'" . fsharp-mode))

(provide 'prelude-fsharp)
