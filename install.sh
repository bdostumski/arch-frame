#!/usr/bin/env bash
#
# -------------------------------------
# ARCH FRAME main INSTALLATION file
# -------------------------------------
#

# -------------------------------------
# External IMPORTS
# -------------------------------------
. "$(dirname "${0}")/dependencies/utils/install-utils.sh"

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
log "\n"
log "USER REGISTRATION:"
log "\n"
log "OS User Name (default: ${USER}): "
read -r USER_NAME
USER_NAME="${USER_NAME:-${USER}}"
log "First Name: "
read -r FIRST_NAME
log "Middle Name: "
read -r MIDDLE_NAME
log "Last Name: "
read -r LAST_NAME
log "Git User: "
read -r GIT_USER
log "Gmail Email: "
read -r GMAIL_EMAIL
log "Gmail User: "
read -r GMAIL_USER
log "Gmail Password: "
stty -echo
read -r GMAIL_PASSWORD
stty echo
printf '\n'
log "Local Database Name (example: database): "
read -r DB_NAME
log "Local Database User Name (example: user): "
read -r DB_USERNAME
log "Local Database Password (example: password): "
stty -echo
read -r DB_PASSWORD
stty echo
printf '\n'
log "\n"

# Validate required fields
if [ -z "${USER_NAME}" ] || [ -z "${GIT_USER}" ] || [ -z "${GMAIL_EMAIL}" ]; then
    log "Error: USER_NAME, GIT_USER, and GMAIL_EMAIL are required." >&2
    exit 1
fi

log "\n"
log "INSTALL DEPENDENCIES:"
log "1) Main Packages Installation [Neovim, Emacs, System, ClamAV, UFW, etc]"
log "2) Dev Packages Installation [Docker, Vagrant, K8s, etc]"
log "3) System Drivers & Firmware Installation"
log "x) Exit"
log "\n"
printf "Enter your choice: "
read -r CHOICE
if [ "${CHOICE}" = "x" ]; then
    log "Exiting..."
    exit 0
fi

DEPENDENCIES_PATH="$(dirname "${0}")/dependencies"

case "${CHOICE}" in
1)
    log "\n"
    log "= = = = = = = = = ="
    log "Main Packages Installation [Neovim, Emacs, System, ClamAV, UFW, etc] . . ."
    . "${DEPENDENCIES_PATH}/pacman-packages.sh"
    . "${DEPENDENCIES_PATH}/git-packages.sh"
    . "${DEPENDENCIES_PATH}/yay-packages.sh"
    . "${DEPENDENCIES_PATH}/doom-emacs.sh"
    log "💡 Restart [exit/start again] rerun the script with kitty terminal"
    ;;
2)
    log "\n"
    log "= = = = = = = = = ="
    log "Dev Packages Installation [Docker, Vagrant, K8s, etc.] . . ."
    . "${DEPENDENCIES_PATH}/dev-tools.sh"
    log "💡 Restart [exit/start again] kitty terminal"
    ;;
3)
    log "\n"
    log "= = = = = = = = = ="
    log "System Drivers & Firmware Installation . . ."
    . "${DEPENDENCIES_PATH}/drivers.sh"
    ;;
*)
    log "\n"
    log "Invalid CHOICE. Please try again." >&2
    exit 1
    ;;
esac

exit 0
