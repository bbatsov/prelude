;; get-file-name.el
;; Commentary:
;; Code:
;; Get user input
(defun my/get-file-name ()
  "Prompt user for a file name with completion and history support.
Insert the file name at the end of buffer."
  (interactive)
  (insert "\n;; The file name is " (read-file-name "Enter the file name: ")))

(my/get-file-name)
;; The file name is ~/.bashrc.bak
;; The file name is ~/.profile
