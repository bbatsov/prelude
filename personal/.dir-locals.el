;; Your personal config isn't a library, and the tools that check Emacs
;; Lisp don't know that.
;;
;; - Nothing in your personal directory is forced through the
;;   emacs-lisp-checkdoc checker of Flycheck.  It's great for real
;;   modules, but these are just config files, and you deserve not to
;;   get warnings all the time.
;; - The byte compiler (which Flycheck runs in a bare Emacs) doesn't
;;   complain about the variables and functions of Prelude and of your
;;   packages, which it knows nothing about.  They are all there by the
;;   time your config is loaded.
((emacs-lisp-mode . ((flycheck-disabled-checkers . (emacs-lisp-checkdoc))
                     (byte-compile-warnings . (not free-vars unresolved)))))
