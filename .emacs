(defconst EMACS_CONF "/Users/liuxingwang/.emacs.d/")

(load-file (concat EMACS_CONF "init.el"))

(load-file (concat EMACS_CONF "elpa/exec-path-from-shell-20150801.104/exec-path-from-shell.el"))
(when (fboundp 'exec-path-from-shell-copy-env)
         (exec-path-from-shell-copy-env "GOPATH")
                (exec-path-from-shell-copy-env "GOBIN")
                         (exec-path-from-shell-copy-env "GOROOT")
                                    (exec-path-from-shell-copy-env "PATH"))

(toggle-frame-fullscreen)

;(set-default-font "Courier 15")
