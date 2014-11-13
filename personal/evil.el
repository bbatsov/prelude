(setq evil-default-state 'normal)
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map
  (read-kbd-macro evil-toggle-key) 'evil-normal-state)

(provide 'evil)
;;; evil.el ends here
