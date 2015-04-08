;;; python-configs -- Python Configs

;;; Code:
;; Use IPython as my python interpreter
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; Enable virtualenvwrapper.el
(venv-initialize-interactive-shells)
(setq venv-location "~/.python_virtualenvs/")

;; Not sure I dig Jedi at the moment. Can't figure out how to get it
;; to place nice with my themes, so it's a bit eye-scalding. Will
;; re-enable later if I figure out the visuals.
;; Jedi-Mode
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook
          (lambda ()
            (whitespace-mode -1)
            (column-enforce-mode)
            (if (bound-and-true-p anaconda-mode)
                (anaconda-mode))
            (setq jedi:complete-on-dot t)
            (add-to-list 'company-backends 'company-jedi)))

;; Virtualenvs
(add-hook 'python-mode-hook
          (lambda ()
            (hack-local-variables)
            (when (boundp 'project-venv-name)
              (venv-workon project-venv-name))))

(add-hook 'venv-postactivate-hook
          (lambda ()
            (let ((env-bin-path (concat (getenv "VIRTUAL_ENV") "bin/postactivate.el")))
              (when (file-exists-p env-bin-path)
                (load env-bin-path)))))

(setq-default mode-line-format (cons '(:exec venv-current-name) mode-line-format))

(provide 'python-configs)

;;; python-configs.el ends here
