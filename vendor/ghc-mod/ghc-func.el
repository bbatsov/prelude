;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-func.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Sep 25, 2009

;;; Code:

(defvar ghc-module-command "ghc-mod"
"*The command name of \"ghc-mod\"")

(defvar ghc-ghc-options nil "*GHC options")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-replace-character (string from to)
  "Replace characters equal to FROM to TO in STRING."
  (let ((ret (copy-sequence string)))
    (dotimes (cnt (length ret) ret)
      (if (char-equal (aref ret cnt) from)
	  (aset ret cnt to)))))

(defun ghc-replace-character-buffer (from-c to-c)
  (let ((from (char-to-string from-c))
	(to (char-to-string to-c)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward from nil t)
	(replace-match to)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ghc-add (sym val)
  `(setq ,sym (cons ,val ,sym)))

(defun ghc-set (vars vals)
  (dolist (var vars)
    (if var (set var (car vals))) ;; var can be nil to skip
    (setq vals (cdr vals))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-filter (pred lst)
  (let (ret)
    (dolist (x lst (reverse ret))
      (if (funcall pred x) (ghc-add ret x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-which (cmd)
  (catch 'loop
    (dolist (suffix '("" ".exe"))
      (let ((cmds (concat cmd suffix)))
	(dolist (dir exec-path)
	  (let ((path (expand-file-name cmds dir)))
	    (if (file-exists-p path)
		(throw 'loop path))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-uniq-lol (lol)
  (let ((hash (make-hash-table :test 'equal))
	ret)
    (dolist (lst lol)
      (dolist (key lst)
	(puthash key key hash)))
    (maphash (lambda (key val) (ghc-add ret key)) hash)
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-read-lisp (func)
  (with-temp-buffer
    (funcall func)
    (goto-char (point-min))
    (condition-case nil
	(read (current-buffer))
      (error ()))))

(defun ghc-read-lisp-list (func n)
  (with-temp-buffer
    (funcall func)
    (goto-char (point-min))
    (condition-case nil
	(let ((m (set-marker (make-marker) 1 (current-buffer)))
	      ret)
	  (dotimes (i n (nreverse ret))
	    (ghc-add ret (read m))))
      (error ()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-mapconcat (func list)
  (apply 'append (mapcar func list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst ghc-null 0)
(defconst ghc-newline 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-things-at-point ()
  (thing-at-point 'sexp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-keyword-number-pair (spec)
  (let ((len (length spec)) key ret)
    (dotimes (i len (nreverse ret))
      (setq key (intern (concat ":" (symbol-name (car spec)))))
      (setq ret (cons (cons key i) ret))
      (setq spec (cdr spec)))))

(defmacro ghc-defstruct (type &rest spec)
  `(progn
     (ghc-defstruct-constructor ,type ,@spec)
     (ghc-defstruct-s/getter ,type ,@spec)))

(defmacro ghc-defstruct-constructor (type &rest spec)
  `(defun ,(intern (concat "ghc-make-" (symbol-name type))) (&rest args)
     (let* ((alist (quote ,(ghc-keyword-number-pair spec)))
	    (struct (make-list (length alist) nil))
	    key val key-num)
       (while args ;; cannot use dolist
	 (setq key  (car args))
	 (setq args (cdr args))
	 (setq val  (car args))
	 (setq args (cdr args))
	 (unless (keywordp key)
	   (error "'%s' is not a keyword" key))
	 (setq key-num (assoc key alist))
	 (if key-num
	     (setcar (nthcdr (cdr key-num) struct) val)
	   (error "'%s' is unknown" key)))
       struct)))

(defmacro ghc-defstruct-s/getter (type &rest spec)
  `(let* ((type-name (symbol-name ',type))
	  (keys ',spec)
	  (len (length keys))
	  member-name setter getter)
     (dotimes (i len)
       (setq member-name (symbol-name (car keys)))
       (setq setter (intern (format "ghc-%s-set-%s" type-name member-name)))
       (fset setter (list 'lambda '(struct value) (list 'setcar (list 'nthcdr i 'struct) 'value) 'struct))
       (setq getter (intern (format "ghc-%s-get-%s" type-name member-name)))
       (fset getter (list 'lambda '(struct) (list 'nth i 'struct)))
       (setq keys (cdr keys)))))

(defun ghc-make-ghc-options ()
  (ghc-mapconcat (lambda (x) (list "-g" x)) ghc-ghc-options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst ghc-error-buffer-name "*GHC Info*")

(defun ghc-display (fontify ins-func)
  (let ((cdir default-directory)
	(buf (get-buffer-create ghc-error-buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (funcall ins-func cdir)
      (ghc-replace-character-buffer ghc-null ghc-newline)
      (goto-char (point-min))
      (if (not fontify)
	  (turn-off-haskell-font-lock)
	(haskell-font-lock-defaults-create)
	(turn-on-haskell-font-lock)))
    (display-buffer buf)))

(provide 'ghc-func)
