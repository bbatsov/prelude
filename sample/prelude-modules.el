;;; Uncomment the modules you'd like to use and restart Prelude afterwards

;;; General productivity tools

;; (require 'prelude-ido) ;; Supercharges Emacs completion for C-x C-f and more
(require 'prelude-ivy) ;; A mighty modern alternative to ido
;; (require 'prelude-helm) ;; Interface for narrowing and search
;; (require 'prelude-helm-everywhere) ;; Enable Helm everywhere
(require 'prelude-company)
;; (require 'prelude-key-chord) ;; Binds useful features to key combinations
;; (require 'prelude-evil)

;;; Org-mode (a legendary productivity tool that deserves its own category)
(require 'prelude-org) ;; Org-mode helps you keep TODO lists, notes and more

;;; Programming languages support

(require 'prelude-c)
;; (require 'prelude-clojure)
;; (require 'prelude-coffee)
;; (require 'prelude-common-lisp)
;; (require 'prelude-css)
;; (require 'prelude-dart)
(require 'prelude-emacs-lisp)
;; (require 'prelude-erlang)
;; (require 'prelude-elixir)
;; (require 'prelude-go)
;; (require 'prelude-haskell)
(require 'prelude-js)
;; (require 'prelude-latex)
(require 'prelude-lisp)
(require 'prelude-lsp)
;; (require 'prelude-ocaml)
(require 'prelude-perl)
;; (require 'prelude-python)
;; (require 'prelude-ruby)
;; (require 'prelude-rust)
;; (require 'prelude-scala)
;; (require 'prelude-scheme)
(require 'prelude-shell)
;; (require 'prelude-scss)
;; (require 'prelude-ts)
;; (require 'prelude-web) ;; Emacs mode for web templates
(require 'prelude-xml)
;; (require 'prelude-yaml)

;;; Misc
(require 'prelude-erc) ;; A popular Emacs IRC client (useful if you're still into Freenode)
