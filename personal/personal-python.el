(prelude-require-packages '(poetry python-black))

;; Stop asking about risky local variable settings.
;; https://emacs.stackexchange.com/a/10989/31688
(add-to-list 'safe-local-variable-values
             '(flycheck-python-flake8-executable . "./venv/bin/flake8"))
(add-to-list 'safe-local-variable-values
             '(flycheck-python-mypy-executable . "./venv/bin/mypy"))

(defun personal-python-mode-defaults ()
  (pyvenv-mode +1)
  (hs-minor-mode +1)
  (setq python-shell-interpreter "ipython"
        ;; https://emacs.stackexchange.com/a/37876/31688
        python-shell-interpreter-args "--simple-prompt --pprint")
  ;; We can jump back to the code using the definition.
  ;; Ref.: https://github.com/emacs-evil/evil/issues/1354
  (evil-set-command-property 'anaconda-mode-find-definitions :jump t)
  (evil-set-command-property 'anaconda-mode-find-assignments :jump t)
  (message "Personal Python default fn executed"))

(add-hook 'python-mode-hook 'personal-python-mode-defaults)
;; Enables function signature in the echo area.
;; https://github.com/pythonic-emacs/anaconda-mode#eldoc
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
