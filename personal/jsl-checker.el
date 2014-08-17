;;; Flycheck for jsl
(eval-after-load "flycheck"
  '(progn
     (flycheck-define-checker javascript-jsl-checker
       "A syntax checker for JavaScript based on jsl. See: https://www.npmjs.org/package/jsl

Requires a local, global install of jsl from npm -- i.e., npm install -g jsl.

Sample checker output:
$ jsl index.js
E ./index.js L56: \"){\n\" should match \") {\"
1 error
checked 1; NOT OK
"
       :command ("/usr/local/bin/jsl" source-inplace)
       :error-patterns ((error line-start "E " (file-name) " L" line ": " (message) line-end))
       :modes (js-mode js2-mode js3-mode)
       :predicate (lambda () (boundp 'use-ua-js)))
     (add-to-list 'flycheck-checkers 'javascript-jsl-checker)))

(provide 'jsl-checker)

;;; jsl-checker ends here
