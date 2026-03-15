#!/bin/sh

has() {
  command -v "$1" >/dev/null 2>&1
}

install_prelude () {
    printf " Cloning Emacs Prelude's GitHub repository...\n${RESET}"
    if [ -n "$PRELUDE_VERBOSE" ]
    then
        if ! /usr/bin/env git clone "$PRELUDE_URL" "$PRELUDE_INSTALL_DIR"
        then
            printf "${RED} A fatal error occurred during Prelude's installation. Aborting...${RESET}\n"
            exit 1
        fi
    else
        if ! /usr/bin/env git clone "$PRELUDE_URL" "$PRELUDE_INSTALL_DIR" > /dev/null 2>&1
        then
            printf "${RED} A fatal error occurred during Prelude's installation. Aborting...${RESET}\n"
            exit 1
        fi
    fi
}

make_prelude_dirs () {
    printf " Creating the required directories.\n${RESET}"
    mkdir -p "$PRELUDE_INSTALL_DIR/savefile"
}

colors_ () {
    case "$SHELL" in
    *zsh)
        autoload colors && colors
        eval RESET='$reset_color'
        for COLOR in RED GREEN YELLOW BLUE MAGENTA CYAN BLACK WHITE
        do
            eval "$COLOR"='$fg_no_bold[${(L)COLOR}]'
            eval "B$COLOR"='$fg_bold[${(L)COLOR}]'
        done
        ;;
    *)
        RESET='\e[0m'
        RED='\e[0;31m'
        GREEN='\e[0;32m'
        YELLOW='\e[0;33m'
        CYAN='\e[0;36m'
        BRED='\e[1;31m'
        BBLUE='\e[1;34m'
        ;;
    esac
}

# Commandline args:
# -d/--directory [dir]
#   Install prelude into the specified directory. If 'dir' is a relative path prefix it with $HOME.
#   Defaults to '$HOME/.emacs.d'
# -c/--colors
#   Enable colors
# -s/--source [url]
#   Clone prelude from 'url'.
#   Defaults to 'https://github.com/bbatsov/prelude.git'
# -i/--into
#   If one exists, install into the existing config
# -n/--no-bytecompile
#   Skip the compilation of the prelude files.
# -h/--help
#   Print help
# -v/--verbose
#   Verbose output, for debugging

usage() {
    printf "Usage: %s [OPTION]\n" "$0"
    printf "  -c, --colors \t \t \t Enable colors.\n"
    printf "  -d, --directory [dir] \t Install Prelude into the specified directory.\n"
    printf "  \t \t \t \t If 'dir' is a relative path prefix with \$HOME.\n"
    printf "  \t \t \t \t Defaults to \$HOME/.emacs.d\n"
    printf "  -s, --source [url] \t \t Clone Prelude from 'url'.\n"
    printf "  \t \t \t \t Defaults to 'https://github.com/bbatsov/prelude.git'.\n"
    printf "  -n, --no-bytecompile \t \t Skip the bytecompilation step of Prelude.\n"
    printf "  -i, --into \t \t \t Install Prelude into a subdirectory in the existing configuration\n"
    printf "  \t \t \t \t The default behavior is to install Prelude into the existing\n"
    printf "  \t \t \t \t Emacs configuration (.emacs.d).\n"
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
            PRELUDE_SKIP_BC='true'
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

if [ -n "$PRELUDE_VERBOSE" ]
then
    printf "${VERBOSE_COLOR}"
    printf "PRELUDE_VERBOSE = %s\n" "$PRELUDE_VERBOSE"
    printf "INSTALL_DIR = %s\n" "$PRELUDE_INSTALL_DIR"
    printf "SOURCE_URL  = %s\n" "$PRELUDE_URL"
    printf "${RESET}"
    if [ -n "$PRELUDE_SKIP_BC" ]
    then
        printf "Skipping bytecompilation.\n"
    fi
    if [ -n "$PRELUDE_INTO" ]
    then
        printf "Replacing existing config (if one exists).\n"
    fi
fi

# If prelude is already installed
if [ -f "$PRELUDE_INSTALL_DIR/core/prelude-core.el" ]
then
    printf "\n\n${BRED}"
    printf "You already have Prelude installed.${RESET}\nYou'll need to remove %s if you want to install Prelude again.\n" "$PRELUDE_INSTALL_DIR"
    printf "If you want to update your copy of Prelude, run 'git pull origin master' from your Prelude directory\n\n"
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

if [ -f "$HOME/.emacs" ]
then
    ## If $HOME/.emacs exists, emacs ignores prelude's init.el, so remove it
    printf " Backing up the existing %s to %s\n" "$HOME/.emacs" "$HOME/.emacs.pre-prelude"
    mv "$HOME/.emacs" "$HOME/.emacs.pre-prelude"
fi

if [ -d "$PRELUDE_INSTALL_DIR" ] || [ -f "$PRELUDE_INSTALL_DIR" ]
then
    # Existing file/directory found -> backup
    printf " Backing up the existing config to %s.pre-prelude.tar.\n" "$PRELUDE_INSTALL_DIR"
    tar -cf "$PRELUDE_INSTALL_DIR.pre-prelude.tar" "$PRELUDE_INSTALL_DIR" > /dev/null 2>&1
    PRELUDE_INSTALL_DIR_ORIG="$PRELUDE_INSTALL_DIR"
    # Overwrite existing?
    [ -n "$PRELUDE_INTO" ] && PRELUDE_INSTALL_DIR="$PRELUDE_INSTALL_DIR/prelude"
    # Clear destination directory for git clone to work
    rm -fr "$PRELUDE_INSTALL_DIR"
    mkdir "$PRELUDE_INSTALL_DIR"
    # Replace existing config
    install_prelude
    make_prelude_dirs
    # Reinstate files that weren't replaced
    tar --skip-old-files -xf "$PRELUDE_INSTALL_DIR_ORIG.pre-prelude.tar" "$PRELUDE_INSTALL_DIR" > /dev/null 2>&1
    [ -n "$PRELUDE_INTO" ] && cp "$PRELUDE_INSTALL_DIR/sample/prelude-modules.el" "$PRELUDE_INSTALL_DIR/personal"
elif [ -e "$PRELUDE_INSTALL_DIR" ]
then
    # File exists but not a regular file or directory
    printf "${BRED} %s exists but isn't a file or directory.\n" "$PRELUDE_INSTALL_DIR"
    printf "${BRED} please remove this file or install Prelude in a different directory"
    printf "${BRED} (-d flag)\n${RESET}"
    exit 1
else
    # Nothing yet so just install prelude
    install_prelude
    make_prelude_dirs
    cp "$PRELUDE_INSTALL_DIR/sample/prelude-modules.el" "$PRELUDE_INSTALL_DIR/personal"
fi

if [ -z "$PRELUDE_SKIP_BC" ]
then
    if has emacs
    then
        printf " Byte-compiling Prelude...\n"
        if [ -n "$PRELUDE_VERBOSE" ]
        then
            emacs -batch -f batch-byte-compile "$PRELUDE_INSTALL_DIR/core"/*.el
        else
            emacs -batch -f batch-byte-compile "$PRELUDE_INSTALL_DIR/core"/*.el > /dev/null 2>&1
        fi
    else
        printf "${YELLOW} Emacs not found.${RESET} Skipping byte-compilation.\n"
    fi
else
    printf "Skipping byte-compilation.\n"
fi

printf "\n"
printf "${BBLUE}  ____           _           _       \n"
printf "${BBLUE} |  _ \ _ __ ___| |_   _  __| | ___  \n"
printf "${BBLUE} | |_) |  __/ _ \ | | | |/ _  |/ _ \ \n"
printf "${BBLUE} |  __/| | |  __/ | |_| | (_| |  __/ \n"
printf "${BBLUE} |_|   |_|  \___|_|\__,_|\__,_|\___| \n\n"
printf "${GREEN} ... is now installed and ready to do thy bidding, Master %s!${RESET}\n" "$USER"
printf "${GREEN} Don't forget to adjust the modules you want to use in %s/personal/prelude-modules.el!${RESET}\n" "$PRELUDE_INSTALL_DIR"
