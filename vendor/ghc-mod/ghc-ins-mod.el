;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-ins-mod.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Dec 27, 2011

;;; Code:

(defvar ghc-hoogle-command "hoogle")

(defun ghc-insert-module ()
  (interactive)
  (if (not (ghc-which ghc-hoogle-command))
      (message "\"%s\" not found" ghc-hoogle-command)
    (let* ((expr0 (ghc-things-at-point))
	   (expr (ghc-read-expression expr0)))
      (let ((mods (ghc-function-to-modules expr)))
	(if (null mods)
	    (message "No module guessed")
	  (let* ((first (car mods))
		 (mod (if (= (length mods) 1)
			  first
			(completing-read "Module name: " mods nil t first))))
	    (save-excursion
	      (ghc-goto-module-position)
	      (insert "import " mod "\n"))))))))

(defun ghc-goto-module-position ()
  (goto-char (point-max))
  (if (re-search-backward "^import" nil t)
      (ghc-goto-empty-line)
    (if (re-search-backward "^module" nil t)
	(ghc-goto-empty-line)
      (goto-char (point-min)))))

(defun ghc-goto-empty-line ()
  (unless (re-search-forward "^$" nil t)
    (forward-line)))

;; To avoid Data.Functor
(defvar ghc-applicative-operators '("<$>" "<$" "<*>" "<**>" "<*" "*>" "<|>"))

(defun ghc-function-to-modules (fn)
  (if (member fn ghc-applicative-operators)
      '("Control.Applicative")
    (ghc-function-to-modules-hoogle fn)))

(defun ghc-function-to-modules-hoogle (fn)
  (with-temp-buffer
    (let* ((fn1 (if (string-match "^[a-zA-Z0-9'_]+$" fn)
		    fn
		  (concat "(" fn ")")))
	   (regex (concat "^\\([a-zA-Z0-9.]+\\) " fn1 " "))
	   ret)
      (call-process ghc-hoogle-command nil t nil "search" fn1)
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
	(setq ret (cons (match-string 1) ret)))
      (nreverse ret))))

(provide 'ghc-ins-mod)
