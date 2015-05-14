;;; Org Mode

;; Make sure auto-fill-mode is on. Pretty much always need it.
(add-hook 'org-mode-hook (lambda () (turn-on-auto-fill)))

;; TODO Keyword states:
;; > In-Progress states: TODO, DOING, BLOCKED
;; > Finished states:    DONE, IMPOSSIBLE, CANCELLED
(setq org-todo-keywords
      '((type "TODO" "DOING" "BLOCKED" "|" "CANCELLED" "IMPOSSIBLE" "DONE")))

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
   (emacs-lisp .t)
   (clojure . t)
   ))

;; Config org export backends
(setq org-export-backends
      `(beamer
        ascii
        markdown
        deck))

;; polymode
(add-to-list 'auto-mode-alist '("\\.org" . poly-org-mode))

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
