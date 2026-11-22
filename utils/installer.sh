#!/bin/sh

has() {
  command -v "$1" >/dev/null 2>&1
}

# Returns non-zero if Prelude couldn't be cloned.
install_prelude () {
    printf " Cloning Emacs Prelude's GitHub repository...\n${RESET}"
    if [ -n "$PRELUDE_VERBOSE" ]
    then
        /usr/bin/env git clone "$PRELUDE_URL" "$PRELUDE_INSTALL_DIR"
    else
        /usr/bin/env git clone "$PRELUDE_URL" "$PRELUDE_INSTALL_DIR" > /dev/null 2>&1
    fi
}

clone_failed () {
    printf "${RED} A fatal error occurred during Prelude's installation. Aborting...${RESET}\n"
    exit 1
}

make_prelude_dirs () {
    printf " Creating the required directories.\n${RESET}"
    mkdir -p "$PRELUDE_INSTALL_DIR/savefile"
}

colors_ () {
    # octal escapes, as not every sh understands \e
    RESET='\033[0m'
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[0;33m'
    CYAN='\033[0;36m'
    BRED='\033[1;31m'
    BBLUE='\033[1;34m'
}

# Commandline args:
# -d/--directory [dir]
#   Install prelude into the specified directory.
#   Defaults to '$HOME/.emacs.d'
# -c/--colors
#   Enable colors
# -s/--source [url]
#   Clone prelude from 'url'.
#   Defaults to 'https://github.com/bbatsov/prelude.git'
# -i/--into
#   If there's a config already, install into its 'prelude' subdirectory
#   instead of replacing it
# -n/--no-bytecompile
#   Does nothing these days, kept so that old invocations still work
# -h/--help
#   Print help
# -v/--verbose
#   Verbose output, for debugging

usage() {
    printf "Usage: %s [OPTION]\n" "$0"
    printf "  -c, --colors \t \t \t Enable colors.\n"
    printf "  -d, --directory [dir] \t Install Prelude into the specified directory.\n"
    printf "  \t \t \t \t Defaults to \$HOME/.emacs.d\n"
    printf "  -s, --source [url] \t \t Clone Prelude from 'url'.\n"
    printf "  \t \t \t \t Defaults to 'https://github.com/bbatsov/prelude.git'.\n"
    printf "  -i, --into \t \t \t If there's an Emacs configuration already, install Prelude\n"
    printf "  \t \t \t \t into its 'prelude' subdirectory.  The default is to replace\n"
    printf "  \t \t \t \t the configuration (after backing it up).\n"
    printf "  -h, --help \t \t \t Display this help and exit\n"
    printf "  -v, --verbose \t \t Display verbose information\n"
    printf "\n"
}

### Parse cli
while [ $# -gt 0 ]
do
    case "$1" in
        -d | --directory)
            PRELUDE_INSTALL_DIR="$2"
            shift 2
            ;;
        -c | --colors)
            colors_
            shift 1
            ;;
        -s | --source)
            PRELUDE_URL="$2"
            shift 2
            ;;
        -i | --into)
            PRELUDE_INTO='true'
            shift 1
            ;;
        -n | --no-bytecompile)
            # Prelude isn't byte-compiled by the installer any more
            shift 1
            ;;
        -h | --help)
            usage
            exit 0
            ;;
        -v | --verbose)
            PRELUDE_VERBOSE='true'
            shift 1
            ;;
        *)
            printf "Unknown option: %s\n" "$1"
            shift 1
            ;;
    esac
done

VERBOSE_COLOR="$BBLUE"

[ -z "$PRELUDE_URL" ] && PRELUDE_URL="https://github.com/bbatsov/prelude.git"
[ -z "$PRELUDE_INSTALL_DIR" ] && PRELUDE_INSTALL_DIR="$HOME/.emacs.d"
# A trailing slash would put the backup inside the directory that gets
# replaced.
while [ "${PRELUDE_INSTALL_DIR%/}" != "$PRELUDE_INSTALL_DIR" ] && [ "$PRELUDE_INSTALL_DIR" != "/" ]
do
    PRELUDE_INSTALL_DIR="${PRELUDE_INSTALL_DIR%/}"
done

if [ -n "$PRELUDE_VERBOSE" ]
then
    printf "${VERBOSE_COLOR}"
    printf "PRELUDE_VERBOSE = %s\n" "$PRELUDE_VERBOSE"
    printf "INSTALL_DIR = %s\n" "$PRELUDE_INSTALL_DIR"
    printf "SOURCE_URL  = %s\n" "$PRELUDE_URL"
    printf "${RESET}"
    if [ -n "$PRELUDE_INTO" ]
    then
        printf "Installing into a subdirectory of the existing config (if one exists).\n"
    else
        printf "Replacing the existing config (if one exists).\n"
    fi
fi

# If prelude is already installed
if [ -f "$PRELUDE_INSTALL_DIR/core/prelude-core.el" ]
then
    printf "\n\n${BRED}"
    printf "You already have Prelude installed.${RESET}\nYou'll need to remove %s if you want to install Prelude again.\n" "$PRELUDE_INSTALL_DIR"
    printf "If you want to update your copy of Prelude, run 'M-x prelude-update' in Emacs.\n\n"
    exit 1
fi

### Check dependencies
printf "${CYAN} Checking to see if git is installed... ${RESET}"
if has git
then
    printf "${GREEN} found.${RESET}\n"
else
    printf "${RED} not found. Aborting installation!${RESET}\n"
    exit 1
fi

printf "${CYAN} Checking to see if aspell is installed... ${RESET}"
if has aspell
then
    printf "${GREEN} found.${RESET}\n"
else
    printf "${RED} not found. Install aspell to benefit from flyspell-mode!${RESET}\n"
fi

### Check emacs version
emacs_version="$(emacs --version 2>/dev/null | sed -n 's/.*[^0-9.]\([0-9]*\.[0-9.]*\).*/\1/p;q' | sed 's/\..*//g')"
if [ "${emacs_version:-0}" -lt 29 ]
then
    printf "${YELLOW} WARNING:${RESET} Prelude requires Emacs ${RED}29${RESET} or newer!\n"
fi

# Emacs only prefers ~/.emacs to the init.el in ~/.emacs.d, so it's left
# alone when Prelude goes somewhere else (e.g. for use with --init-directory)
if [ -f "$HOME/.emacs" ] && [ "$PRELUDE_INSTALL_DIR" = "$HOME/.emacs.d" ] && [ -z "$PRELUDE_INTO" ]
then
    ## If $HOME/.emacs exists, emacs ignores prelude's init.el, so remove it
    printf " Backing up the existing %s to %s\n" "$HOME/.emacs" "$HOME/.emacs.pre-prelude"
    mv "$HOME/.emacs" "$HOME/.emacs.pre-prelude"
fi

if [ -L "$PRELUDE_INSTALL_DIR" ]
then
    # The backup would hold nothing but the link, and the link would be
    # replaced with a directory.
    printf "${BRED} %s is a symbolic link.\n" "$PRELUDE_INSTALL_DIR"
    printf "${BRED} Remove the link or install Prelude in a different directory"
    printf "${BRED} (-d flag)\n${RESET}"
    exit 1
elif [ -d "$PRELUDE_INSTALL_DIR" ] || [ -f "$PRELUDE_INSTALL_DIR" ]
then
    # Existing file/directory found -> backup
    PRELUDE_INSTALL_DIR_ORIG="$PRELUDE_INSTALL_DIR"
    PRELUDE_BACKUP="$PRELUDE_INSTALL_DIR_ORIG.pre-prelude.tar"
    if [ -e "$PRELUDE_BACKUP" ]
    then
        # most likely the backup of an install that didn't finish, and
        # as such the only copy of the old config
        printf "${BRED} There's a backup from an earlier install already: %s\n" "$PRELUDE_BACKUP"
        printf "${BRED} Move it out of the way and run the installer again.\n${RESET}"
        exit 1
    fi
    printf " Backing up the existing config to %s.\n" "$PRELUDE_BACKUP"
    # The archive holds paths relative to the parent directory, so that
    # it can be extracted in place later on.
    if ! tar -cf "$PRELUDE_BACKUP" \
         -C "$(dirname "$PRELUDE_INSTALL_DIR_ORIG")" "$(basename "$PRELUDE_INSTALL_DIR_ORIG")" > /dev/null 2>&1
    then
        # the next step deletes the config, so there's no going on without a backup
        printf "${RED} Couldn't back up %s. Aborting...${RESET}\n" "$PRELUDE_INSTALL_DIR_ORIG"
        exit 1
    fi
    # Overwrite existing?
    [ -n "$PRELUDE_INTO" ] && PRELUDE_INSTALL_DIR="$PRELUDE_INSTALL_DIR/prelude"
    # Clear destination directory for git clone to work
    rm -fr "$PRELUDE_INSTALL_DIR"
    mkdir "$PRELUDE_INSTALL_DIR"
    # Replace existing config
    if ! install_prelude
    then
        # put things back the way they were
        rm -fr "$PRELUDE_INSTALL_DIR"
        if tar -k -xf "$PRELUDE_BACKUP" -C "$(dirname "$PRELUDE_INSTALL_DIR_ORIG")" > /dev/null 2>&1
        then
            rm -f "$PRELUDE_BACKUP"
            printf " Restored %s from the backup.\n" "$PRELUDE_INSTALL_DIR_ORIG"
        else
            printf "${RED} Your old config is in %s.${RESET}\n" "$PRELUDE_BACKUP"
        fi
        clone_failed
    fi
    make_prelude_dirs
    # Reinstate files that weren't replaced.  -k keeps the files that
    # are there already; GNU tar reports those as errors, hence no check.
    tar -k -xf "$PRELUDE_BACKUP" -C "$(dirname "$PRELUDE_INSTALL_DIR_ORIG")" > /dev/null 2>&1
    # Emacs would load the old config's compiled init files instead of
    # Prelude's
    [ -z "$PRELUDE_INTO" ] && rm -f "$PRELUDE_INSTALL_DIR/init.elc" "$PRELUDE_INSTALL_DIR/early-init.elc"
    # an old config that was a Prelude might have brought its own
    [ -f "$PRELUDE_INSTALL_DIR/personal/prelude-modules.el" ] ||
        cp "$PRELUDE_INSTALL_DIR/sample/prelude-modules.el" "$PRELUDE_INSTALL_DIR/personal"
elif [ -e "$PRELUDE_INSTALL_DIR" ]
then
    # File exists but not a regular file or directory
    printf "${BRED} %s exists but isn't a file or directory.\n" "$PRELUDE_INSTALL_DIR"
    printf "${BRED} please remove this file or install Prelude in a different directory"
    printf "${BRED} (-d flag)\n${RESET}"
    exit 1
else
    # Nothing yet so just install prelude
    install_prelude || clone_failed
    make_prelude_dirs
    cp "$PRELUDE_INSTALL_DIR/sample/prelude-modules.el" "$PRELUDE_INSTALL_DIR/personal"
fi

printf "\n"
printf "${BBLUE}  ____           _           _       \n"
printf "${BBLUE} |  _ \ _ __ ___| |_   _  __| | ___  \n"
printf "${BBLUE} | |_) |  __/ _ \ | | | |/ _  |/ _ \ \n"
printf "${BBLUE} |  __/| | |  __/ | |_| | (_| |  __/ \n"
printf "${BBLUE} |_|   |_|  \___|_|\__,_|\__,_|\___| \n\n"
printf "${GREEN} ... is now installed and ready to do thy bidding, Master %s!${RESET}\n" "$USER"
printf "${GREEN} Don't forget to adjust the modules you want to use in %s/personal/prelude-modules.el!${RESET}\n" "$PRELUDE_INSTALL_DIR"
