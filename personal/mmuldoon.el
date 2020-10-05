(global-set-key (kbd "C-S-SPC") 'easy-mark)
(global-set-key (kbd "S-<f10>") 'menu-bar-open)
(global-set-key (kbd "S-<f12>") 'menu-bar-mode)
(global-set-key (kbd "s-<backspace>") 'crux-kill-line-backwards)
(global-set-key (kbd "M-s-<backspace>") 'kill-line)
(global-set-key (kbd "<f5>") 'org-capture)
(global-set-key (kbd "M-<f5>") 'org-roam-capture)
(prelude-require-packages '(indium ng2-mode clj-refactor discover-clj-refactor org-roam org-download company-org-roam))
(setq prelude-format-on-save t)
(add-hook 'js2-mode-hook #'(lambda() (setq js-indent-level 2)))
(add-hook 'after-init-hook 'org-roam-mode)
(add-hook 'markdown-mode-hook #'(lambda() (setq markdown-command "multimarkdown")))
(add-hook 'dired-mode-hook 'org-download-enable)
(add-hook 'org-mode-hook #'turn-on-auto-fill)
(setq org-directory "~/Documents/6_Notes"
      org-ellipsis " ▼ ")
(setq-default org-download-image-dir "./_resources")

(setq org-roam-directory "~/Documents/6_Notes")
(setq org-roam-title-sources '((title) alias))
(setq mmuldoon/org-directory (file-truename "~/Documents/6_Notes"))
(setq mmuldoon/agenda-subdirs '("/private-notes/" "/todo/" "/mmuldoon-work/" "/org-roam/"))
(setq org-agenda-files
      (mapcar #'(lambda(subdir)
                  (concat org-directory subdir)) mmuldoon/agenda-subdirs))
(add-hook 'org-mode-hook #'(lambda () (setq org-default-notes-file (concat org-directory "/inbox.org"))))
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(org-babel-do-load-languages
 'org-babel-load-languages '((shell . t)
                             (clojure . t)
                             (js . t)))
(defun +org-search ()
  (interactive)
  (org-refile '(4)))

;;; Require

(eval-and-compile (require 'org)) ; for `org-completing-read'

;;; Code:

(defvar ocv/capture-prmt-history nil
  "History of prompt answers for Org capture.")
(defvar ocv/capture-prmt-type-history nil
  "History of prompt answers for Org capture.")

(defun ocv/prmt (prompt variable &optional default history)
  "PROMPT for string, save it to VARIABLE and insert it.
Optional argument DEFAULT, a string, is the default input value, if any.
Optional argument HISTORY, a symbol bound to a completion list (of strings)
to offer.  It defaults to `ocv/capture-prmt-history'."
  (set (make-local-variable variable)
       (let* ((history (or history 'ocv/capture-prmt-history))
              (hist-list (symbol-value history)))
         (org-completing-read
          (concat prompt ": ") hist-list nil nil nil history default))))

(setq org-capture-templates
      `(("t" "Todo" entry (file+headline ,(concat org-directory "/inbox.org") "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree ,(concat org-directory "/private-notes/journal.org"))
         "* %?")
        ("m" "Meeting" entry (file+datetree ,(concat org-directory "/mmuldoon-work/meetings.org"))
         "* Topic: %?\n** Type: %(ocv/prmt \"Type\" 'type nil 'ocv/capture-prmt-type-history)\n** Purpose: \n** Present:\n- Mickey\n** Notes:\n")
        ("p" "Private meeting" entry (file+datetree ,(concat org-directory "/private-notes/meetings.org")) "* Topic: %?\n** Type: %(ocv/prmt \"Type\" 'type nil 'ocv/capture-prmt-type-history)\n** Purpose:\n** Present:\n- Mickey\n** Notes:\n")))

(require 'company-org-roam)
(push 'company-org-roam company-backends)

(setq projectile-project-search-path '("~/Documents/2_Areas/OSCAR/Oscar_Code" "~/Documents/2_Areas/Ochoa/" "~/Documents/2_Areas/Ripley/Ripley_Code" "~/Documents/2_Areas/Dex/Dex_Code" "~/Documents/6_Notes"))

(require 'clj-refactor)

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
(setq sentence-end-double-space nil)
