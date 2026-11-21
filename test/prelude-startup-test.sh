#!/bin/sh
# Start Prelude in a real (non-batch) session, the way a user would, and
# fail if that produces any warnings or errors.
#
# Batch mode skips quite a bit of the startup (the welcome screen, the
# after-init hooks of some packages, anything that's guarded by
# `noninteractive'), and it doesn't show the *Warnings* buffer that a
# user would get in their face.
#
# Usage: test/prelude-startup-test.sh [extra emacs arguments]

server=prelude-startup-test

emacs "$@" --daemon=$server > /dev/null 2>&1 || {
  echo "[test] FAILED - Emacs didn't start"
  exit 1
}

problems=$(emacsclient -s $server --eval '
(let ((problems nil))
  (when init-file-had-error
    (push "init.el signaled an error" problems))
  (unless (featurep (quote prelude-editor))
    (push "the core of Prelude was not loaded" problems))
  (when (get-buffer "*Warnings*")
    (push (with-current-buffer "*Warnings*" (buffer-substring-no-properties (point-min) (point-max))) problems))
  (mapconcat (function identity) problems "\n"))')

emacsclient -s $server --eval '(kill-emacs)' > /dev/null 2>&1

if [ "$problems" = '""' ]; then
  echo "[test] PASSED - Prelude started without warnings."
else
  echo "[test] FAILED - problems on startup:"
  printf '%b\n' "$problems"
  exit 1
fi
