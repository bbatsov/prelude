;; This buffer is for text that is not saved, and for Lisp evaluation.
;; practice user input
;; Sun 27 Aug 2023 05:59:58 AM +03
;; Commentary:
;; Code:
(defun my/get-file-name ()
  "Prompt user to enter a file name with completion and history support."
  (interactive)
  ;;  (message "String is %s" (read-file-name "Enter file name: ")))
  ;; note: no need for formatting %s since the function is insert, not message
  (insert "\n;; The file name is " (read-file-name "Enter file name: ")))

(my/get-file-name)
;; The file name is ~/GithubRepos/magnars.emacs.d/README.md
;; The file name is ~/temp/luccas.readme.org
;; The file name is ~/.profile
;; The file name is~/bin/emacs.listing.txt
;; String is %s~/.vimrc
