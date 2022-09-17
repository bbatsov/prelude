(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(blink-cursor-mode nil)
 '(cider-clojure-cli-aliases "dev:test:cljs")
 '(column-number-mode t)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(css-indent-offset 2)
 '(custom-safe-themes
   '("6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" "7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9" "9e3ea605c15dc6eb88c5ff33a82aed6a4d4e2b1126b251197ba55d6b86c610a1" "7eea50883f10e5c6ad6f81e153c640b3a288cd8dc1d26e4696f7d40f754cc703" "6b1abd26f3e38be1823bd151a96117b288062c6cde5253823539c6926c3bb178" "d6603a129c32b716b3d3541fc0b6bfe83d0e07f1954ee64517aa62c9405a3441" "2c49d6ac8c0bf19648c9d2eabec9b246d46cb94d83713eaae4f26b49a8183fc4" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "aaa4c36ce00e572784d424554dcc9641c82d1155370770e231e10c649b59a074" "c086fe46209696a2d01752c0216ed72fd6faeabaaaa40db9fc1518abebaf700d" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "711efe8b1233f2cf52f338fd7f15ce11c836d0b6240a18fffffc2cbd5bfe61b0" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" "978b8e73997ece5e2e8705e62dce6f712554aebeb6e16c442dd361ce029d850f" "f7676de79f95bf4b1e3cc680529121deaee848cc583ebca317415e8bc26447fd" default))
 '(desktop-save-mode t)
 '(evil-symbol-word-search t)
 '(fci-rule-color "#383838")
 '(fill-column 79)
 '(helm-completion-style 'emacs)
 '(helm-minibuffer-history-key "M-p")
 '(jdee-db-active-breakpoint-face-colors (cons "#0d0d0d" "#81a2be"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#0d0d0d" "#b5bd68"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#0d0d0d" "#5a5b5a"))
 '(js-indent-level 2)
 '(js2-strict-missing-semi-warning nil)
 '(keyboard-coding-system 'utf-8-unix)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(objed-cursor-color "#cc6666")
 '(org-agenda-files
   '("~/Codes/Blogging/org-notes/notes.org" "~/Codes/Blogging/org-notes/tasks.org"))
 '(org-todo-keywords '((sequence "TODO" "DOING" "|" "DONE")))
 '(package-selected-packages
   '(haskell-mode lsp-grammarly python-black gruvbox-theme mermaid-mode lua-mode json-navigator nodejs-repl protobuf-mode org-drill yasnippet-snippets yasnippet poetry flycheck-clj-kondo dockerfile-mode org-preview-html org-preview-html-mode scss-mode org-bullets tide origami cython-mode yaml-mode evil-vimish-fold vimish-fold neotree idle-highlight-mode doom-themes evil-collection json-mode eglot-fsharp fsharp-mode rust-mode key-chord prelude-evil helm-ag helm-descbinds helm-projectile helm exec-path-from-shell zop-to-char zenburn-theme which-key volatile-highlights undo-tree super-save smartrep smartparens operate-on-number move-text magit projectile imenu-anywhere hl-todo guru-mode gitignore-mode gitconfig-mode git-timemachine gist flycheck expand-region epl editorconfig easy-kill diminish diff-hl discover-my-major crux browse-kill-ring beacon anzu ace-window))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(prelude-auto-save nil)
 '(prelude-format-on-save nil)
 '(prelude-mode t)
 '(rustic-ansi-faces
   ["#1d1f21" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#c9b4cf" "#8abeb7" "#c5c8c6"])
 '(safe-local-variable-values
   '((eval when
           (require 'rainbow-mode nil t)
           (rainbow-mode 1))))
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(typescript-indent-level 2)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(warning-suppress-types '((comp)))
 '(web-mode-markup-indent-offset 2)
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
