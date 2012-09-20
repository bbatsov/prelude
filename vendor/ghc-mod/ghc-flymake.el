;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-flymake.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar 12, 2010

;;; Code:

(require 'flymake)
(require 'ghc-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ghc-hlint-options nil "*Hlint options")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst ghc-flymake-allowed-file-name-masks
  '("\\.l?hs$" ghc-flymake-init))

(defconst ghc-flymake-err-line-patterns
  '("^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\):[ ]*\\(.+\\)" 1 2 3 4))

(add-to-list 'flymake-allowed-file-name-masks
	     ghc-flymake-allowed-file-name-masks)

(add-to-list 'flymake-err-line-patterns
	     ghc-flymake-err-line-patterns)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-flymake-init ()
  (list ghc-module-command (ghc-flymake-command (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))))

(defvar ghc-flymake-command nil) ;; nil: check, t: lint

(defun ghc-flymake-command (file)
   (if ghc-flymake-command
       (let ((hopts (ghc-mapconcat (lambda (x) (list "-h" x)) ghc-hlint-options)))
	 `(,@hopts "lint" ,file))
     `(,@(ghc-make-ghc-options) "check" ,file)))

(defun ghc-flymake-toggle-command ()
  (interactive)
  (setq ghc-flymake-command (not ghc-flymake-command))
  (if ghc-flymake-command
      (message "Syntax check with hlint")
    (message "Syntax check with GHC")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-flymake-display-errors ()
  (interactive)
  (if (not (ghc-flymake-have-errs-p))
      (message "No errors or warnings")
    (let ((title (ghc-flymake-err-title))
	  (errs (ghc-flymake-err-list)))
      (ghc-display
       nil
       (lambda (&rest ignore)
	 (insert title "\n\n")
	 (mapc (lambda (x) (insert x "\n")) errs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-flymake-jump ()
  (interactive)
  (if (not (ghc-flymake-have-errs-p))
      (message "No errors or warnings")
    (let* ((acts (ghc-flymake-act-list))
	   (act (car acts)))
      (if (not act)
	  (message "No destination")
	(eval act)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-extract-type (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (when (re-search-forward "Inferred type: \\|no type signature:\\( \\|\0 +\\)?" nil t)
      (delete-region (point-min) (point)))
    (when (re-search-forward " forall [^.]+\\." nil t)
      (replace-match ""))
    (while (re-search-forward "\0 +" nil t)
      (replace-match " "))
    (goto-char (point-min))
    (while (re-search-forward "\\[Char\\]" nil t)
      (replace-match "String"))
    (re-search-forward "\0" nil t)
    (buffer-substring-no-properties (point-min) (1- (point)))))

(defun ghc-flymake-insert-from-warning ()
  (interactive)
  (dolist (data (ghc-flymake-err-list))
    (save-excursion
      (cond
       ((string-match "Inferred type: \\|no type signature:" data)
	(beginning-of-line)
	(insert (ghc-extract-type data) "\n"))
       ((string-match "lacks an accompanying binding" data)
	(beginning-of-line)
	(when (looking-at "^\\([^ ]+\\) *::")
	  (save-match-data
	    (forward-line)
	    (if (not (bolp)) (insert "\n")))
	  (insert (match-string 1) " = undefined\n")))
       ((string-match "Not in scope: `\\([^']+\\)'" data)
	(save-match-data
	  (unless (re-search-forward "^$" nil t)
	    (goto-char (point-max))
	    (insert "\n")))
	(insert "\n" (match-string 1 data) " = undefined\n"))
       ((string-match "Pattern match(es) are non-exhaustive" data)
	(let* ((fn (ghc-get-function-name))
	       (arity (ghc-get-function-arity fn)))
	  (ghc-insert-underscore fn arity)))
       ((string-match "Found:\0[ ]*\\([^\0]+\\)\0Why not:\0[ ]*\\([^\0]+\\)" data)
	(let ((old (match-string 1 data))
	      (new (match-string 2 data)))
	  (beginning-of-line)
	  (when (search-forward old nil t)
	    (let ((end (point)))
	      (search-backward old nil t)
	      (delete-region (point) end))
	    (insert new))))))))

(defun ghc-get-function-name ()
  (save-excursion
    (beginning-of-line)
    (when (looking-at "\\([^ ]+\\) ")
      (match-string 1))))

(defun ghc-get-function-arity (fn)
  (when fn
    (save-excursion
      (let ((regex (format "^%s *::" (regexp-quote fn))))
	(when (re-search-backward regex nil t)
	  (ghc-get-function-arity0))))))

(defun ghc-get-function-arity0 ()
  (let ((end (save-excursion (end-of-line) (point)))
	(arity 0))
    (while (search-forward "->" end t)
      (setq arity (1+ arity)))
    arity))

(defun ghc-insert-underscore (fn ar)
  (when fn
    (let ((arity (or ar 1)))
      (save-excursion
	(goto-char (point-max))
	(re-search-backward (format "^%s *::" (regexp-quote fn)))
	(forward-line)
	(re-search-forward "^$" nil t)
	(insert fn)
	(dotimes (i arity)
	  (insert " _"))
	(insert  " = error \"" fn "\"")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-flymake-err-get-title (x) (nth 0 x))
(defun ghc-flymake-err-get-errs (x) (nth 1 x))

(defun ghc-flymake-err-get-err-msg (x) (nth 0 x))
(defun ghc-flymake-err-get-err-act (x) (nth 1 x))

(defalias 'ghc-flymake-have-errs-p 'ghc-flymake-data)

(defun ghc-flymake-data ()
  (let* ((line-no (flymake-current-line-no))
         (info (nth 0 (flymake-find-err-info flymake-err-info line-no))))
    (flymake-make-err-menu-data line-no info)))

(defun ghc-flymake-err-title ()
  (ghc-flymake-err-get-title (ghc-flymake-data)))

(defun ghc-flymake-err-list ()
  (mapcar 'ghc-flymake-err-get-err-msg (ghc-flymake-err-get-errs (ghc-flymake-data))))

(defun ghc-flymake-act-list ()
  (mapcar 'ghc-flymake-err-get-err-act (ghc-flymake-err-get-errs (ghc-flymake-data))))

(provide 'ghc-flymake)
