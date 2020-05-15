(prelude-require-packages '(key-chord))

(require 'prelude-evil)
(require 'key-chord)

;; Alternative escape key definition.
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode 1)
