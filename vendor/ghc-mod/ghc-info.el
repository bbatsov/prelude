;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-info.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Nov 15, 2010

;;; Code:

(require 'ghc-func)

(defun ghc-show-info (&optional ask)
  (interactive "P")
  (let* ((modname (or (ghc-find-module-name) "Main"))
	 (expr0 (ghc-things-at-point))
	 (expr (if ask (ghc-read-expression expr0) expr0))
	 (file (buffer-file-name))
	 (cmds (list "info" file modname expr)))
    (ghc-display-information cmds nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; type
;;;

(defvar ghc-type-overlay nil)

(make-variable-buffer-local 'ghc-type-overlay)

(defun ghc-type-set-ix (n)
  (overlay-put ghc-type-overlay 'ix n))

(defun ghc-type-get-ix ()
  (overlay-get ghc-type-overlay 'ix))

(defun ghc-type-set-point (pos)
  (overlay-put ghc-type-overlay 'pos pos))

(defun ghc-type-get-point ()
  (overlay-get ghc-type-overlay 'pos))

(defun ghc-type-set-types (types)
  (overlay-put ghc-type-overlay 'types types))

(defun ghc-type-get-types ()
  (overlay-get ghc-type-overlay 'types))

(ghc-defstruct tinfo beg-line beg-column end-line end-column info)

(defun ghc-type-init ()
  (setq ghc-type-overlay (make-overlay 0 0))
  (overlay-put ghc-type-overlay 'face 'region)
  (ghc-type-clear-overlay)
  (setq after-change-functions
	(cons 'ghc-type-clear-overlay after-change-functions))
  (add-hook 'post-command-hook 'ghc-type-post-command-hook))

(defun ghc-type-clear-overlay (&optional beg end len)
  (when (overlayp ghc-type-overlay)
    (ghc-type-set-ix 0)
    (ghc-type-set-point 0)
    (move-overlay ghc-type-overlay 0 0)))

(defun ghc-type-post-command-hook ()
  (when (and (eq major-mode 'haskell-mode)
	     (overlayp ghc-type-overlay)
	     (/= (ghc-type-get-point) (point)))
    (ghc-type-clear-overlay)))

(defun ghc-show-type ()
  (interactive)
  (if (not (ghc-which ghc-module-command))
      (message "%s not found" ghc-module-command)
    (let ((modname (or (ghc-find-module-name) "Main")))
      (ghc-show-type0 modname))))

(defun ghc-show-type0 (modname)
  (let* ((buf (current-buffer))
	 (tinfos (ghc-type-get-tinfos modname)))
    (if (null tinfos)
	(progn
	  (ghc-type-clear-overlay)
	  (message "Cannot guess type"))
      (let* ((tinfo (nth (ghc-type-get-ix) tinfos))
	     (type (ghc-tinfo-get-info tinfo))
	     (beg-line (ghc-tinfo-get-beg-line tinfo))
	     (beg-column (ghc-tinfo-get-beg-column tinfo))
	     (end-line (ghc-tinfo-get-end-line tinfo))
	     (end-column (ghc-tinfo-get-end-column tinfo))
	     (left (ghc-get-pos buf beg-line beg-column))
	     (right (ghc-get-pos buf end-line end-column)))
	(move-overlay ghc-type-overlay (- left 1) (- right 1) buf)
	(message type)))))

(defun ghc-type-get-tinfos (modname)
  (if (= (ghc-type-get-point) (point))
      (ghc-type-set-ix
       (mod (1+ (ghc-type-get-ix)) (length (ghc-type-get-types))))
    (ghc-type-set-types (ghc-type-obtain-tinfos modname))
    (ghc-type-set-point (point))
    (ghc-type-set-ix 0))
  (ghc-type-get-types))

(defun ghc-type-obtain-tinfos (modname)
  (let* ((ln (int-to-string (line-number-at-pos)))
	 (cn (int-to-string (current-column)))
	 (cdir default-directory)
	 (file (buffer-file-name)))
    (ghc-read-lisp
     (lambda ()
       (cd cdir)
       (apply 'call-process ghc-module-command nil t nil
	      `(,@(ghc-make-ghc-options) "-l" "type" ,file ,modname ,ln ,cn))
       (goto-char (point-min))
       (while (search-forward "[Char]" nil t)
	 (replace-match "String"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Expanding Template Haskell
;;;

(defun ghc-expand-th ()
  (interactive)
  (let* ((file (buffer-file-name))
	 (cmds (list "expand" file)))
    (ghc-display-information cmds t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Display
;;;

(defun ghc-display-information (cmds fontify)
  (interactive)
  (if (not (ghc-which ghc-module-command))
      (message "%s not found" ghc-module-command)
    (ghc-display
     fontify
     (lambda (cdir)
       (insert
	(with-temp-buffer
	  (cd cdir)
	  (apply 'call-process ghc-module-command nil t nil
		 (append (ghc-make-ghc-options) cmds))
	  (buffer-substring (point-min) (1- (point-max)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Misc
;;;

(defun ghc-get-pos (buf line col)
  (save-excursion
    (set-buffer buf)
    (goto-line line)
    (forward-char col)
    (point)))

(defun ghc-read-expression (default)
  (if default
      (let ((prompt (format "Expression (%s): " default)))
	(read-string prompt default nil))
    (read-string "Expression: ")))

(defun ghc-find-module-name ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^module[ ]+\\([^ \n]+\\)" nil t)
	(match-string-no-properties 1))))

(provide 'ghc-info)
