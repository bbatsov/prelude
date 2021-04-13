(require 'prelude-programming)

(prelude-require-packages '(fsharp-mode eglot-fsharp origami))

(with-eval-after-load 'fsharp-mode
  (setq inferior-fsharp-program "dotnet fsi --readline-")
  (setq-default fsharp-indent-offset 2)
  ;; WARNING: I couldn't make "eglot-fsharp" work out-of-box. I had to
  ;; manually download "FsAutoComplete" and move to a directory
  ;; structure similar to the one automatically created by "eglot-fsharp".
  (require 'eglot-fsharp)

  ;; ----- Folding with "origami" -----
  (add-hook 'fsharp-mode-hook 'origami-mode)

  ;; ----- Formatting functions -----
  ;; Credits to: https://zwild.github.io/posts/emacs-as-a-fsharp-ide-part1/.
  (require 'projectile)

  (defun fsharp-format-code-region (start end)
    "Formats a code region interactively using Fantomas.
Ref.: https://github.com/fsprojects/fantomas."
    (interactive "r")
    (let* ((source (shell-quote-argument (buffer-substring-no-properties start end)))
           (tool-name "fantomas")
           (ok-buffer (format "*%s*" tool-name))
           (error-buffer (format "*%s-errors*" tool-name)))
      (save-window-excursion
        (projectile-with-default-dir (projectile-acquire-root)
          (shell-command-on-region
           start
           end
           ;; TODO: the option "--stdin" seems to ignore ".editorconfig".
           ;; https://github.com/fsprojects/fantomas/pull/940#issuecomment-653449468
           (format "dotnet %s %s --stdout" tool-name (buffer-file-name))
           ok-buffer
           nil
           ;; The error buffer is used only to quickly check if the
           ;; command failed.
           error-buffer))
        (if (get-buffer error-buffer)
            ;; If the error buffer is created, show its contents before
            ;; killing the buffer.
            (progn
              (with-current-buffer error-buffer
                (message (buffer-string)))
              (kill-buffer error-buffer))
          (delete-region start end)
          (insert (with-current-buffer ok-buffer
                    (buffer-string)))
          (message "Code formatted")))))

  (defun fsharp-format-code-buffer ()
    "Formats the code in a buffer using Fantomas."
    (interactive)
    (let ((origin (point)))
      (fsharp-format-code-region (point-min) (point-max))
      (goto-char origin)))

  (evil-define-key 'normal fsharp-mode-map
    ",f" 'fsharp-format-code-buffer)
  (evil-define-key 'visual fsharp-mode-map
    ",f" 'fsharp-format-code-region))

(provide 'personal-fsharp)
