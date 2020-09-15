(global-set-key (kbd "C-S-SPC") 'easy-mark)
(global-set-key (kbd "S-<f10>") 'menu-bar-open)
(global-set-key (kbd "S-<f12>") 'menu-bar-mode)
(global-set-key (kbd "s-<backspace>") 'crux-kill-line-backwards)
(global-set-key (kbd "M-s-<backspace>") 'kill-line)
(global-set-key (kbd "<f5>") 'org-capture)
(prelude-require-packages '(indium ng2-mode clj-refactor discover-clj-refactor org-roam))
(setq prelude-format-on-save t)
(add-hook 'js2-mode-hook #'(lambda() (setq js-indent-level 2)))
(add-hook 'markdown-mode-hook #'(lambda() (setq markdown-command "multimarkdown")))
(setq org-directory "~/Documents/6_Notes")
(setq mmuldoon/org-directory (file-truename "~/Documents/6_Notes"))
(setq mmuldoon/agenda-subdirs '("/notes/" "/private-notes/" "/todo/"))
(setq org-agenda-files
      (mapcar #'(lambda(subdir)
                  (concat org-directory subdir)) mmuldoon/agenda-subdirs))
(add-hook 'org-mode-hook #'(lambda () (setq org-default-notes-file (concat org-directory "/inbox-note.org"))))
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(defun +org-search ()
  (interactive)
  (org-refile '(4)))

(setq org-capture-templates
      `(("t" "Todo" entry (file+headline ,(concat org-directory "/inbox.org") "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree ,(concat org-directory "/private-notes/journal.org"))
         "* %?")
        ("m" "Meeting" entry (file+datetree ,(concat org-directory "/notes/meetings.org")) "* Topic: %?\n** Present")
        ))


(add-hook 'projectile-mode-hook #'(lambda() (setq projectile-project-search-path ("~/Documents/2_Areas/OSCAR/Oscar_Code" "~/Documents/2_Areas/Ochoa/" "~/Documents/2_Areas/Ripley/Ripley_Code" "~/Documents/2_Areas/Dex/Dex_Code" "~/Documents/6_Notes"))))

(require 'clj-refactor)

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
(setq sentence-end-double-space nil)
