;; ;; Evil-Mode Configs -- Summary

;; ;;; Code
;; (setq evil-default-state 'normal)
;; (setcdr evil-insert-state-map nil)
;; (define-key evil-insert-state-map
;;   (read-kbd-macro evil-toggle-key) 'evil-normal-state)
;; (define-key evil-insert-state-map
;;   (kbd "<escape>") 'evil-normal-state)

;; ;;; Set certain modes to certain states
;; (evil-set-initial-state 'git-commit-mode 'insert)
;; (provide 'evil)
;; ;;; evil.el ends here
