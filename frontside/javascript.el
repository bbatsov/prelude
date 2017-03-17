(prelude-require-packages '(js2-refactor json-mode mocha-snippets))

;; We use js2r-refactor-mode which implies using js2-mode.
;; see https://github.com/magnars/js2-refactor.el
;;
;; all refactorings start with C-c C-r (for refactor!)
(js2r-add-keybindings-with-prefix "C-c C-r")

(custom-set-variables '(js-indent-level 2)
                      '(js2-basic-offset 2))
