;; this includes the smart-modeline which enhances the modeline to show
;; the most relevant information.
;;
;; Also, we use rich-minority mode to filter out modes like "yas" or
;; "whitespace" which are always in effect, and don't contribute
;; that much extra information.
;;
;; see:
;;  https://github.com/Bruce-Connor/smart-mode-line/
;;  https://github.com/Bruce-Connor/rich-minority
(prelude-require-packages '(smart-mode-line rich-minority pcre2el))
(setq sml/no-confirm-load-theme t)
(sml/setup)

(add-hook 'after-init-hook (lambda ()
                             (custom-set-variables
                              '(rm-blacklist (rxt-pcre-to-elisp  "(guru|drag|yas|FlyC|company|ws|Pre|Sp/s|SP|Projectile|MRev)")))))
