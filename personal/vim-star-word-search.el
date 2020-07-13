;; Search for word under cursor
;; press C-* to initiate (sorta like vim)
;; http://stackoverflow.com/questions/589691/how-can-i-emulate-vims-search-in-gnu-emacs
(defun my-isearch-word-at-point ()
  (interactive)
  (call-interactively 'isearch-forward-regexp))

(defun my-isearch-yank-word-hook ()
  (when (equal this-command 'my-isearch-word-at-point)
    (let ((string (concat "\\<"
                          (buffer-substring-no-properties
                           (progn (skip-syntax-backward "w_") (point))
                           (progn (skip-syntax-forward "w_") (point)))
                          "\\>")))
      (if (and isearch-case-fold-search
               (eq 'not-yanks search-upper-case))
          (setq string (downcase string)))
      (setq isearch-string string
            isearch-message
            (concat isearch-message
                    (mapconcat 'isearch-text-char-description
                               string ""))
            isearch-yank-flag t)
      (isearch-search-and-update))))

(add-hook 'isearch-mode-hook 'my-isearch-yank-word-hook)
(global-set-key (kbd "C-*") 'my-isearch-word-at-point)
