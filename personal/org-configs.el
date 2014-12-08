;;; Org Mode

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
