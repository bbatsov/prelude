#!/bin/sh
# Runs utils/installer.sh against a throwaway home directory and checks
# what it leaves behind.  Prelude is cloned from this checkout, so
# nothing is downloaded.
#
# Usage: test/prelude-installer-test.sh

prelude="$(cd "$(dirname "$0")/.." && pwd)"
installer="$prelude/utils/installer.sh"
failures=0

check() {
    # check <description> <test arguments...>
    description="$1"; shift
    if [ "$@" ]
    then
        printf "[test]   ok: %s\n" "$description"
    else
        printf "[test]   FAILED: %s\n" "$description"
        failures=$((failures + 1))
    fi
}

homes=""
cleanup() {
    for home in $homes
    do
        case "$home" in
            */prelude-installer-test.*) rm -rf "$home" ;;
        esac
    done
}
trap cleanup EXIT

new_home() {
    HOME="$(mktemp -d "${TMPDIR:-/tmp}/prelude-installer-test.XXXXXX")"
    export HOME
    homes="$homes $HOME"
}

install() {
    if ! "${PRELUDE_TEST_SH:-sh}" "$installer" -s "$prelude" "$@" > "$HOME/installer.log" 2>&1
    then
        printf "[test]   FAILED: the installer exited with an error\n"
        cat "$HOME/installer.log"
        failures=$((failures + 1))
    fi
}

printf "[test] A fresh install\n"
new_home
install
check "Prelude is in ~/.emacs.d" -f "$HOME/.emacs.d/core/prelude-core.el"
check "the modules file is in personal/" -f "$HOME/.emacs.d/personal/prelude-modules.el"
check "nothing got byte-compiled" -z "$(find "$HOME/.emacs.d" -name '*.elc')"

printf "[test] Replacing an existing config\n"
new_home
mkdir -p "$HOME/.emacs.d/snippets"
echo "old init" > "$HOME/.emacs.d/init.el"
echo "old bytecode" > "$HOME/.emacs.d/init.elc"
echo "a snippet" > "$HOME/.emacs.d/snippets/mine"
echo "old dot emacs" > "$HOME/.emacs"
install -c
check "Prelude replaced the old init.el" -f "$HOME/.emacs.d/core/prelude-core.el"
check "the old init.el didn't come back" "$(cat "$HOME/.emacs.d/init.el")" != "old init"
check "there's a backup" -f "$HOME/.emacs.d.pre-prelude.tar"
check "the backup has the old init.el" \
      "$(tar -xOf "$HOME/.emacs.d.pre-prelude.tar" .emacs.d/init.el 2> /dev/null)" = "old init"
check "files Prelude doesn't have are reinstated" \
      "$(cat "$HOME/.emacs.d/snippets/mine" 2> /dev/null)" = "a snippet"
check "the old init.elc isn't, as Emacs would prefer it to Prelude's init.el" \
      ! -e "$HOME/.emacs.d/init.elc"
check "the modules file is in personal/" -f "$HOME/.emacs.d/personal/prelude-modules.el"
check "the colors (-c) are escape sequences this sh understands" \
      -z "$(grep -F -e '\033' -e '\e[' "$HOME/installer.log")"
check "~/.emacs is out of the way" ! -e "$HOME/.emacs"
check "~/.emacs was backed up" "$(cat "$HOME/.emacs.pre-prelude" 2> /dev/null)" = "old dot emacs"

printf "[test] Installing into an existing config\n"
new_home
mkdir -p "$HOME/.emacs.d"
echo "old init" > "$HOME/.emacs.d/init.el"
echo "dot emacs" > "$HOME/.emacs"
install -i
check "Prelude is in a subdirectory" -f "$HOME/.emacs.d/prelude/core/prelude-core.el"
check "the old init.el is still there" "$(cat "$HOME/.emacs.d/init.el")" = "old init"
check "~/.emacs is left alone" "$(cat "$HOME/.emacs" 2> /dev/null)" = "dot emacs"

printf "[test] A directory given with a trailing slash\n"
new_home
mkdir -p "$HOME/.emacs.d"
echo "old init" > "$HOME/.emacs.d/init.el"
install -d "$HOME/.emacs.d/"
check "Prelude replaced the old config" -f "$HOME/.emacs.d/core/prelude-core.el"
check "the backup is next to the directory, not in it" \
      "$(tar -xOf "$HOME/.emacs.d.pre-prelude.tar" .emacs.d/init.el 2> /dev/null)" = "old init"

printf "[test] A clone that fails\n"
new_home
mkdir -p "$HOME/.emacs.d"
echo "old init" > "$HOME/.emacs.d/init.el"
if "${PRELUDE_TEST_SH:-sh}" "$installer" -s "$HOME/no-such-repo" > /dev/null 2>&1
then
    printf "[test]   FAILED: the installer reported success\n"
    failures=$((failures + 1))
fi
check "the old config is back in place" "$(cat "$HOME/.emacs.d/init.el" 2> /dev/null)" = "old init"

printf "[test] A backup left by an earlier run\n"
new_home
mkdir -p "$HOME/.emacs.d"
echo "old init" > "$HOME/.emacs.d/init.el"
echo "precious" > "$HOME/.emacs.d.pre-prelude.tar"
if "${PRELUDE_TEST_SH:-sh}" "$installer" -s "$prelude" > /dev/null 2>&1
then
    printf "[test]   FAILED: the installer went ahead\n"
    failures=$((failures + 1))
fi
check "the earlier backup is untouched" "$(cat "$HOME/.emacs.d.pre-prelude.tar")" = "precious"
check "and so is the config" "$(cat "$HOME/.emacs.d/init.el" 2> /dev/null)" = "old init"

printf "[test] A config that's a symbolic link\n"
new_home
mkdir -p "$HOME/dotfiles/emacs"
echo "old init" > "$HOME/dotfiles/emacs/init.el"
ln -s "$HOME/dotfiles/emacs" "$HOME/.emacs.d"
if "${PRELUDE_TEST_SH:-sh}" "$installer" -s "$prelude" > /dev/null 2>&1
then
    printf "[test]   FAILED: the installer went ahead\n"
    failures=$((failures + 1))
fi
check "the link is still a link" -L "$HOME/.emacs.d"
check "its target is untouched" "$(cat "$HOME/dotfiles/emacs/init.el" 2> /dev/null)" = "old init"

printf "[test] Installing somewhere else\n"
new_home
echo "dot emacs" > "$HOME/.emacs"
install -d "$HOME/prelude"
check "Prelude is where it was told to go" -f "$HOME/prelude/core/prelude-core.el"
check "~/.emacs is left alone" "$(cat "$HOME/.emacs" 2> /dev/null)" = "dot emacs"

printf "[test] Refusing to install over Prelude\n"
if "${PRELUDE_TEST_SH:-sh}" "$installer" -s "$prelude" -d "$HOME/prelude" > /dev/null 2>&1
then
    printf "[test]   FAILED: installed over an existing Prelude\n"
    failures=$((failures + 1))
else
    printf "[test]   ok: stopped with an error\n"
fi

if [ "$failures" -eq 0 ]
then
    printf "\n[test] PASSED - the installer does what it says.\n"
else
    printf "\n[test] FAILED - %d problem(s).\n" "$failures"
    exit 1
fi
