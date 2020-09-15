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
(add-hook 'org-mode-hook #'(lambda() (setq org-agenda-files '("~/Documents/6_Notes/notes/" "~/Documents/6_Notes/private-notes/" "~/Documents/6_Notes/private-notes" "~/Documents/6_Notes/Inbox/"))))
(add-hook 'org-mode-hook #'(lambda() (setq org-directory "~/Documents/6_Notes")))
(add-hook 'org-mode-hook #'(lambda () (setq org-default-notes-file (concat org-directory "/inbox-note.org"))))
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(defun +org-search ()
  (interactive)
  (org-refile '(4)))


(add-hook 'projectile-mode-hook #'(lambda() (setq projectile-project-search-path ("~/Documents/2_Areas/OSCAR/Oscar_Code" "~/Documents/2_Areas/Ochoa/" "~/Documents/2_Areas/Ripley/Ripley_Code" "~/Documents/2_Areas/Dex/Dex_Code"))))

(require 'clj-refactor)

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
(setq sentence-end-double-space nil)
