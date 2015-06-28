;;; Org Mode

;; Make sure auto-fill-mode is on. Pretty much always need it.
;;(require 'org)
(require 'org-bullets)
(add-hook 'org-mode-hook
          (lambda ()
            (turn-on-auto-fill)
            (org-bullets-mode 1)))

;; TODO Keyword states:
;; > In-Progress states: TODO, DOING, BLOCKED
;; > Finished states:    DONE, IMPOSSIBLE, CANCELLED
(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(o)" "|" "DONE(d)")
        (sequence "BLOCKED(b)" "|" "UNBLOCKED (u)" "CANCELLED(c)" "IMPOSSIBLE(i)")))

(setq org-todo-keyword-faces
      '(("TODO" . org-todo)
        ("DOING" . org-todo)
        ("BLOCKED" . org-warning)
        ("CANCELLED" . org-done)
        ("IMPOSSIBLE" . org-done)
        ("DONE" . org-done)))

;; Support for Babel Mode code blocks
;; NOTE: requires the addition of the org elpa repo!
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp . t)
   (java . t)
   (sh . t)
   (R . t)
   (scala . t)
   (sql . t)))

;; Smartparens pairs!
(sp-local-pair 'org-mode "~" "~" :wrap "C-~")
(sp-local-pair 'org-mode "/" "/")
(sp-local-pair 'org-mode "*" "*" :wrap "C-*")

;; Config org export backends
(setq org-export-backends
      `(beamer
        ascii
        md
        deck
        html))

;; Export defaults: no table of contents, no numbered headers, don't convert ^
;; or _ to superscripts
(setq org-export-with-section-numbers nil
      org-export-with-sub-superscripts nil
      org-export-with-toc nil)

;; Refiling defaults
(setq org-refile-targets '((org-agenda-files :maxlevel . 3))
      org-refile-allow-creating-parent-nodes 'confirm)

;; For reasons I can't grok at all, Prelude seems to disable some org keyboard
;; shortcuts. Let's fix that.
(defun org-bindings ()
  (define-key prelude-mode-map (kbd "C-c /") 'org-sparse-tree)
  (define-key prelude-mode-map [(control shift return)] 'org-insert-todo-heading-respect-content))

(add-hook 'org-mode-hook 'org-bindings)

;; Hide org emphasis marks
(setq org-hide-emphasis-markers t)

;; Start indented
(setq org-startup-indented t)

;; Stop folding. Just... stop.
(setq org-startup-folded nil)

;; Fontify inside code blocks
(setq org-src-fontify-natively t)

;; org-mime for composing emails
(require 'org-mime)

(provide 'org-configs)

;;; org-configs ends her
