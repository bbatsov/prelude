;;; abizern.el --- Emacs Prelude: Personal settings
;;

;; Set Menlo 12 as my default font
(set-default-font "-apple-Menlo-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1")
(modify-frame-parameters nil '((wait-for-wm . nil)))

;; Set up what my default browser is
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; Load my paths so that eshell knows what I can do
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell
         (replace-regexp-in-string "[[:space:]\n]*$" ""
                                   (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))

;; Make sure there is a final newline
(setq-default require-final-newline t)

;; Get rid of trailing white space on saves
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Turn off long line highlighting.
(setq whitespace-line-column 250)

;; But it would be nice to autoflow long paragraphs
(setq-default fill-column 80)

;; Tell gist to use curl
(setq gist-use-curl t)

;; Remove hooks from prelude
(remove-hook 'message-mode-hook 'prelude-turn-on-flyspell)
(remove-hook 'text-mode-hook 'prelude-turn-on-flyspell)

;; Remove flyspell from prog-mode
(defun fix-prelude-prog-mode-defaults ()
  (prelude-turn-off-whitespace)
  (turn-off-flyspell))

(add-hook 'prelude-prog-mode-hook 'fix-prelude-prog-mode-defaults t)

;; Add back arrow keys support
(prelude-restore-arrow-keys)

;; Keybindings

;;; I like to use shell, not eshell
(global-set-key (kbd "C-x m") 'shell)

;; YASnippet

;;; Remove binding of <tab> key from Markdown cycle
;;; This way I can use YASnippets in my markdown files.
(defun markdown-unset-tab ()
  "markdown-mode-hook"
  (define-key markdown-mode-map (kbd "<tab>") nil))
(add-hook 'markdown-mode-hook '(lambda() (markdown-unset-tab)))

;;; Use ido-mode for choosing snippets
(setq yas/prompt-functions '(yas/ido-prompt yas/dropdown-prompt))

;; Enable erase-buffer
(put 'erase-buffer 'disabled nil)

;; If I'm running emacs, then I want it to be a server
(server-start)
