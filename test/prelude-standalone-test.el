;;; prelude-standalone-test.el --- Tests for modules loaded on their own -*- lexical-binding: t; -*-

;;; Commentary:

;; The completion modules, Evil and key-chord change Emacs globally, and
;; some of them can't be loaded together, so CI loads each of them in an
;; Emacs of its own and runs these tests there.  A test runs only when
;; its module has been loaded.
;;
;; Usage:
;;   emacs --batch -l init.el --eval "(require 'prelude-evil)" \
;;     -l test/prelude-standalone-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

(defvar company-idle-delay)
(defvar evil-normal-state-map)
(defvar evil-visual-state-map)
(defvar helm-command-map)
(defvar global-company-mode)
(defvar global-evil-surround-mode)
(defvar global-evil-visualstar-mode)
(defvar evil-mode)
(defvar evil-emacs-state-cursor)
(defvar helm-mode)
(defvar ido-mode)
(defvar ido-ubiquitous-mode)
(defvar flx-ido-mode)
(defvar ivy-mode)
(defvar key-chord-mode)

(ert-deftest prelude-company-completes-everywhere ()
  (skip-unless (featurep 'prelude-company))
  (should global-company-mode)
  (should (eql company-idle-delay 0.5))
  ;; diminished
  (should (equal (cadr (assq 'company-mode minor-mode-alist)) "")))

(ert-deftest prelude-ido-takes-over-completion ()
  (skip-unless (featurep 'prelude-ido))
  (should (eq ido-mode 'both))
  (should ido-ubiquitous-mode)
  (should flx-ido-mode)
  (should (eq (key-binding (kbd "M-x")) 'smex)))

(ert-deftest prelude-ivy-takes-over-completion ()
  (skip-unless (featurep 'prelude-ivy))
  (should ivy-mode)
  (should (eq (key-binding (kbd "C-s")) 'swiper))
  (should (eq (key-binding (kbd "M-x")) 'counsel-M-x))
  (should (eq (lookup-key minibuffer-local-map (kbd "C-r")) 'counsel-minibuffer-history)))

(ert-deftest prelude-ivy-keeps-off-the-keys-of-others ()
  "No user keys (C-c and a letter) and no standard commands replaced."
  (skip-unless (featurep 'prelude-ivy))
  (should (eq (key-binding (kbd "<f6>")) 'ivy-resume))
  (let ((ivy-commands nil))
    (dolist (char (append (number-sequence ?a ?z) (number-sequence ?A ?Z)))
      (let ((command (key-binding (vector ?\C-c char))))
        (when (and (symbolp command)
                   (string-match-p "\\`\\(?:counsel\\|ivy\\|swiper\\)-" (symbol-name command)))
          (push (cons (string char) command) ivy-commands))))
    (should-not ivy-commands))
  (should (eq (key-binding (kbd "C-x l")) 'count-lines-page)))

(ert-deftest prelude-helm-commands-are-under-c-c-h ()
  "Even without Projectile, which used to load Helm for the module."
  (skip-unless (featurep 'prelude-helm))
  (should (eq (key-binding (kbd "C-c h")) 'helm-command-prefix))
  (should-not (key-binding (kbd "C-x c")))
  (should (eq (lookup-key helm-command-map (kbd "o")) 'helm-occur)))

(ert-deftest prelude-helm-everywhere-takes-over-completion ()
  (skip-unless (featurep 'prelude-helm-everywhere))
  (should helm-mode)
  (should (eq (key-binding (kbd "M-x")) 'helm-M-x))
  (should (eq (lookup-key isearch-mode-map (kbd "C-o")) 'helm-occur-from-isearch)))

(ert-deftest prelude-evil-turns-on-its-extensions ()
  (skip-unless (featurep 'prelude-evil))
  (should evil-mode)
  (should (equal evil-emacs-state-cursor '("red" box)))
  (should global-evil-surround-mode)
  ;; installed, but never turned on before
  (should global-evil-visualstar-mode)
  (should (eq (lookup-key evil-normal-state-map (kbd "C-A")) 'evil-numbers/inc-at-pt))
  (should (eq (lookup-key evil-visual-state-map (kbd ">")) 'prelude-shift-right-visual)))

(ert-deftest prelude-key-chord-defines-its-chords ()
  (skip-unless (featurep 'prelude-key-chord))
  (should key-chord-mode)
  (should (eq (lookup-key (current-global-map) [key-chord ?j ?j]) 'avy-goto-word-1)))

(provide 'prelude-standalone-test)
;;; prelude-standalone-test.el ends here
