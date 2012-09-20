;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Sep 25, 2009

(require 'ghc-func)
(require 'ghc-comp)

;;; Code:

(defun ghc-browse-document (&optional haskell-org)
  (interactive "P")
  (let* ((mod0 (ghc-extract-module))
	 (mod (ghc-read-module-name mod0))
	 (pkg (ghc-resolve-package-name mod)))
    (if (and pkg mod)
	(ghc-display-document pkg mod haskell-org)
      (message "No document found"))))

(defun ghc-resolve-package-name (mod)
  (with-temp-buffer
    (call-process "ghc-pkg" nil t nil "find-module" "--simple-output" mod)
    (goto-char (point-min))
    (when (re-search-forward "\\([^ ]+\\)-\\([0-9]*\\(\\.[0-9]+\\)*\\)$" nil t)
      (ghc-make-pkg-ver
       :pkg (match-string-no-properties 1)
       :ver (match-string-no-properties 2)))))

(defun ghc-resolve-document-path (pkg)
  (with-temp-buffer
    (call-process "ghc-pkg" nil t nil "field" pkg "haddock-html")
    (goto-char (point-max))
    (forward-line -1)
    (beginning-of-line)
    (when (looking-at "^haddock-html: \\([^ \n]+\\)$")
      (match-string-no-properties 1))))

(defconst ghc-doc-local-format "file://%s/%s.html")
(defconst ghc-doc-hackage-format
  "http://hackage.haskell.org/packages/archive/%s/%s/doc/html/%s.html")

(ghc-defstruct pkg-ver pkg ver)

(defun ghc-display-document (pkg-ver mod haskell-org)
  (when (and pkg-ver mod)
    (let* ((mod- (ghc-replace-character mod ?. ?-))
	   (pkg (ghc-pkg-ver-get-pkg pkg-ver))
	   (ver (ghc-pkg-ver-get-ver pkg-ver))
	   (pkg-with-ver (format "%s-%s" pkg ver))
	   (path (ghc-resolve-document-path pkg-with-ver))
	   (local (format ghc-doc-local-format path mod-))
	   (remote (format ghc-doc-hackage-format pkg ver mod-))
	   (file (format "%s/%s.html" path mod-))
           (url (if (or haskell-org (not (file-exists-p file))) remote local)))
      (browse-url url))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ghc-input-map nil)

(unless ghc-input-map
  (setq ghc-input-map
	(if (boundp 'minibuffer-local-map)
	    (copy-keymap minibuffer-local-map)
	  (make-sparse-keymap)))
  (define-key ghc-input-map "\t" 'ghc-complete))

(defun ghc-read-module-name (def)
  (read-from-minibuffer "Module name: " def ghc-input-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-extract-module ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^\\(import\\|module\\) +\\(qualified +\\)?\\([^ (\n]+\\)")
	(match-string-no-properties 3))))

(provide 'ghc-doc)
