;; Emacs creates lockfiles to recognize when someone else is already
;; editing the same file as you.
;;
;; Ember-CLI doesn't know what to do with these lock files. One second
;; they are there and the next the lock file disappears. This causes
;; issues with Ember-CLI's livereload feature where you will commonly
;; get an error like:
;;
;; Error: ENOENT, no such file or directory '.../components/.#file-name.hbs'
;;
;; To solve this issue we set "create-lockfiles" to nil and it will no
;; longer create these lock files.


(setq create-lockfiles nil)
