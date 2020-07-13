(defun +org|setup-ui () 
  "Configures the UI for `org-mode'." 
  (setq-default org-adapt-indentation nil 
                org-cycle-include-plain-lists t 
                org-eldoc-breadcrumb-separator " → " 
                org-entities-user '(("flat" "\\flat" nil "" "" "266D" "♭") ("sharp" "\\sharp" nil "" "" "266F" "♯")) 
                org-fontify-done-headline t 
                org-fontify-quote-and-verse-blocks t 
                org-fontify-whole-heading-line t 
                org-footnote-auto-label 'plain 
                org-hidden-keywords nil 
                org-hide-emphasis-markers nil 
                org-hide-leading-stars t 
                org-hide-leading-stars-before-indent-mode t 
                org-image-actual-width nil 
                org-indent-indentation-per-level 2 
                org-indent-mode-turns-on-hiding-stars t 
                org-list-description-max-indent 4 
                org-pretty-entities nil 
                org-pretty-entities-include-sub-superscripts t 
                org-priority-faces '((?a . error) (?b . warning) (?c . success)) 
                org-refile-targets '((nil :maxlevel . 3) (org-agenda-files :maxlevel . 3)) 
                org-startup-folded t 
                org-startup-indented t 
                org-startup-with-inline-images nil 
                org-tags-column 0
                org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)") (sequence "[ ](T)" "[-](p)" "[?](m)" "|" "[X](D)") (sequence "NEXT(n)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)")) 
                org-todo-keyword-faces '(("[-]" :inherit (font-lock-constant-face bold)) ("[?]" :inherit (warning bold)) ("WAITING" :inherit bold) ("LATER" :inherit (warning bold)))
                org-use-sub-superscripts '{}))

(defun my-org-setup ()
  (setq org-hide-emphasis-markers t)
  (setq org-startup-indented t)
  (setq truncate-lines nil)
  (setq word-wrap t)
  (org-indent-mode)
  (linum-mode t))

(add-hook 'org-mode-hook 'my-org-setup)
