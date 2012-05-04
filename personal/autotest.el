;;; autotest.el --- ZenTest's autotest integration with emacs.

;; Copyright (C) 2006-2007 by Ryan Davis

;; Author: Ryan Davis <ryand-ruby@zenspider.com>
;; Version 1.0
;; Keywords: testing, ruby, convenience
;; Created: 2006-11-17
;; Compatibility: Emacs 22, 21?
;; URL(en): http://seattlerb.rubyforge.org/
;; by Ryan Davis - ryand-ruby@zenspider.com

;;; Posted using:
;; (emacswiki-post "autotest.el")

;;; The MIT License:

;; http://en.wikipedia.org/wiki/MIT_License
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; Sets up an autotest buffer and provides convenience methods.

;;; History:
;; 1.0.0 - 2008-09-25 - Added an extra regexp for rspec/mspec. 1.0.0 release.
;; 1.0b4 - 2007-09-25 - Added autotest-use-ui and autotest-command vars.
;; 1.0b3 - 2007-05-10 - emacs compatibility fixes and improved regexps.
;; 1.0b2 - 2007-04-03 - added autotest plugin / communication support
;; 1.0b1 - 2007-03-06 - initial release

(require 'shell)

(defcustom autotest-use-ui nil
  "Should we use test-unit's UI?"
  :group 'autotest
  :type '(boolean))

(defcustom autotest-command "autotest"
  "Command name to use to execute autotest."
  :group 'autotest
  :type '(string))

;;;###autoload
(defun autotest ()
  "Fire up an instance of autotest in its own buffer with shell bindings and compile-mode highlighting and linking."
  (interactive)
  (let ((buffer (shell "*autotest*")))

    (define-key shell-mode-map "\C-c\C-a" 'autotest-switch)

    (set (make-local-variable 'comint-output-filter-functions)
         '(comint-truncate-buffer comint-postoutput-scroll-to-bottom))
    (set (make-local-variable 'comint-buffer-maximum-size) 5000)
    (set (make-local-variable 'comint-scroll-show-maximum-output) t)
    (set (make-local-variable 'comint-scroll-to-bottom-on-output) t)

    (set (make-local-variable 'compilation-error-regexp-alist)
         '(
           ("^ +\\(#{RAILS_ROOT}/\\)?\\([^(:]+\\):\\([0-9]+\\)" 2 3)
           ("\\[\\(.*\\):\\([0-9]+\\)\\]:$" 1 2)
           ("^ *\\([[+]\\)?\\([^:
]+\\):\\([0-9]+\\):in" 2 3)
           ("^.* at \\([^:]*\\):\\([0-9]+\\)$" 1 2)
           ))
    (compilation-shell-minor-mode)
    (comint-send-string buffer (concat autotest-command "\n"))))

(defun autotest-switch ()
  "Switch back and forth between autotest and the previous buffer"
  (interactive)
  (if (equal "*autotest*" (buffer-name))
      (switch-to-buffer (other-buffer))
    (switch-to-buffer "*autotest*")))

(eval-when-compile
  (require 'unit-test nil t))

(if (and autotest-use-ui (require 'unit-test nil t))
    (progn
      (message "starting emacs server for autotest")
      (setq unit-test-colours (acons "gray" "#999999" unit-test-colours))
      (setq unit-test-colours (acons "dark-gray" "#666666" unit-test-colours))
      (setq unit-test-running-xpm (unit-test-dot "gray"))
      (server-start)
      (defun autotest-update (status)
        "Updates all buffer's modeline with the current test status."
        (interactive "S")
        (let ((autotest-map (make-sparse-keymap)))
          (define-key autotest-map [mode-line mouse-1] 'autotest-switch)
          (mapcar (lambda (buffer)
                    (with-current-buffer buffer
                      (if (eq status 'quit)
                          (show-test-none)
                        (progn
                          (show-test-status status)
                          (put-text-property
                           0 3
                           'keymap autotest-map
                           (car mode-line-buffer-identification))))))
                  (remove-if 'minibufferp (buffer-list))))
        status))
  (message "unit-test not found, not starting autotest/emacs integration"))

(provide 'autotest)
