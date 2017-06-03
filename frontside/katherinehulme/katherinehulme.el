(load-theme 'deeper-blue)
(prelude-require-packages '(rvm))
(require 'rvm)
(rvm-autodetect-ruby)

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)
(setq backup-directory-alist `((".*" . "~/.emacs.d/autosaves")))
