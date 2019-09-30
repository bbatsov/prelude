(prelude-require-packages '(flycheck 'flycheck-golangci-lint))
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))
(setq flycheck-golangci-lint-fast t)
(setq flycheck-golangci-lint-disable-all t)
(setq flycheck-golangci-lint-deadline "1m")
(setq flycheck-golangci-lint-enable-linters '("govet" "errcheck" "staticcheck" "unused" "gosimple" "structcheck" "varcheck" "ineffassign" "deadcode" "typecheck" "bodyclose" "golint" "stylecheck" "gosec" "interfacer" "unconvert"
                                              "dupl" "gofmt" "maligned" "depguard" "misspell" "unparam" "nakedret" "prealloc" "scopelint" "gocritic" "whitespace"))
