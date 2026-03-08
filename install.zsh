#!/usr/bin/env zsh
#
# -------------------------------------
# ARCH FRAME main INSTALLATION file
# -------------------------------------
#

# -------------------------------------
# External IMPORTS
# -------------------------------------
source "$(dirname "${0}")/dependencies/utils/install-utils.zsh"

log "\n"
log " ▗▄▖ ▗▄▄▖  ▗▄▄▖▗▖ ▗▖    ▗▄▄▄▖▗▄▄▖  ▗▄▖ ▗▖  ▗▖▗▄▄▄▖ "
log "▐▌ ▐▌▐▌ ▐▌▐▌   ▐▌ ▐▌    ▐▌   ▐▌ ▐▌▐▌ ▐▌▐▛▚▞▜▌▐▌    "
log "▐▛▀▜▌▐▛▀▚▖▐▌   ▐▛▀▜▌    ▐▛▀▀▘▐▛▀▚▖▐▛▀▜▌▐▌  ▐▌▐▛▀▀▘ "
log "▐▌ ▐▌▐▌ ▐▌▝▚▄▄▖▐▌ ▐▌    ▐▌   ▐▌ ▐▌▐▌ ▐▌▐▌  ▐▌▐▙▄▄▖ "
log "\n"
log "Let's try to master the chaos 🔥"
log "Created by Borislav Aleksandrov Dostumski"
log "\n"
log "This is the first of its sort, Linux configuration framework 🛠️"
log "But first you have to install and run zsh"
log "\n"
log "Highly recommended is to try it on a virtual machine, or to install it on a fresh Arch installation"
log "\n"

# TODO: User Registration Process:
# USERNAME
# FIRST_NAME
# MIDDLE_NAME
# LAST_NAME
# GIT_USERNAME
# GMAIL
# GMAIL_USER
# GMAIL_PASSWORD
# DB_NAME
# DB_USERNAME
# DB_PASSWORD

log "Install Dependencies 📦"
log "1) Main Packages Installation [Neovim, Emacs, System, ClamAV, UFW, etc]"
log "2) Dev Packages Installation [Docker, Vagrant, K8s, etc.]"
log "3) System Drivers & Firmware Installation"
log "x) Exit"
log "\n"
echo -n "Enter your choise: "
read -r CHOICE
if [ "${CHOICE}" = "x" ]; then
    log "Exiting..."
    exit 0
fi

DEPENDENCIES_PATH="$(dirname ${0})/dependencies"

case "${CHOICE}" in
1)
    log "\n"
    log "= = = = = = = = = ="
    log "Main Packages Installation [Neovim, Emacs, System, ClamAV, UFW, etc] . . ."
    source "${DEPENDENCIES_PATH}/pacman-packages.zsh"
    source "${DEPENDENCIES_PATH}/git-packages.zsh"
    source "${DEPENDENCIES_PATH}/yay-packages.zsh"
    source "${DEPENDENCIES_PATH}/doom-emacs.zsh"
    log "💡 Restart [exit/start again] rerun the script with kitty terminal"
    ;;
2)
    log "\n"
    log "= = = = = = = = = ="
    log "Dev Packages Installation [Docker, Vagrant, K8s, etc.] . . ."
    source "${DEPENDENCIES_PATH}/dev-tools.zsh"
    log "💡 Restart [exit/start again] kitty terminal"
    ;;
3)
    log "\n"
    log "= = = = = = = = = ="
    log "System Drivers & Firmware Installation . . ."
    source "${DEPENDENCIES_PATH}/drivers.zsh"
    ;;
*)
    log "\n"
    log -e "Invalid CHOICE. Please try again." ">&2"
    exit 1
    ;;
esac

exit 0
