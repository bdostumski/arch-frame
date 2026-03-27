#!/usr/bin/env sh
#
# -------------------------------------
# ARCH FRAME main INSTALLATION file
# -------------------------------------
#

# -------------------------------------
# External IMPORTS
# -------------------------------------
. "$(dirname "${0}")/dependencies/utils/install-utils.sh"

# -------------------------------------
# Restore terminal on interrupt
# -------------------------------------
trap 'stty echo; exit 1' INT TERM

printf '\n'
log " ▗▄▖ ▗▄▄▖  ▗▄▄▖▗▖ ▗▖    ▗▄▄▄▖▗▄▄▖  ▗▄▖ ▗▖  ▗▖▗▄▄▄▖ "
log "▐▌ ▐▌▐▌ ▐▌▐▌   ▐▌ ▐▌    ▐▌   ▐▌ ▐▌▐▌ ▐▌▐▛▚▞▜▌▐▌    "
log "▐▛▀▜▌▐▛▀▚▖▐▌   ▐▛▀▜▌    ▐▛▀▀▘▐▛▀▚▖▐▛▀▜▌▐▌  ▐▌▐▛▀▀▘ "
log "▐▌ ▐▌▐▌ ▐▌▝▚▄▄▖▐▌ ▐▌    ▐▌   ▐▌ ▐▌▐▌ ▐▌▐▌  ▐▌▐▙▄▄▖ "
printf '\n'
log "Let's try to master the chaos 🔥"
log "Created by Borislav Aleksandrov Dostumski"
printf '\n'
log "This is the first of its sort, Linux configuration framework 🛠️"
log "But first you have to install and run zsh"
printf '\n'
log "Highly recommended is to try it on a virtual machine, or to install it on a fresh Arch installation"
printf '\n'
printf '\n'

# -------------------------------------
# USER REGISTRATION
# -------------------------------------
printf "USER REGISTRATION:\n"
printf '\n'
printf "OS User Name: "
read -r USER_NAME
printf "First Name: "
read -r FIRST_NAME
printf "Middle Name: "
read -r MIDDLE_NAME
printf "Last Name: "
read -r LAST_NAME
printf "Git User: "
read -r GIT_USER
printf "Gmail Email: "
read -r GMAIL_EMAIL
printf "Gmail User: "
read -r GMAIL_USER
printf "Gmail Password: "
stty -echo
read -r GMAIL_PASSWORD
stty echo
printf '\n'
printf "Local Database Name (example: database): "
read -r DB_NAME
printf "Local Database User Name (example: user): "
read -r DB_USERNAME
printf "Local Database Password (example: password): "
stty -echo
read -r DB_PASSWORD
stty echo
printf '\n'
printf '\n'

# -------------------------------------
# Validate required fields
# -------------------------------------
if [ -z "${USER_NAME}" ] || [ -z "${GIT_USER}" ] || [ -z "${GMAIL_EMAIL}" ]; then
    log "Error: USER_NAME, GIT_USER, and GMAIL_EMAIL are required." >&2
    exit 1
fi

# -------------------------------------
# INSTALL DEPENDENCIES MENU
# -------------------------------------
printf '\n'
log "INSTALL DEPENDENCIES:"
log "1) Main Packages Installation [Neovim, Emacs, System, ClamAV, UFW, etc]"
log "2) Dev Packages Installation [Docker, Vagrant, K8s, etc]"
log "3) System Drivers & Firmware Installation"
log "x) Exit"
printf '\n'
printf "Enter your choice: "
read -r CHOICE

if [ "${CHOICE}" = "x" ]; then
    log "Exiting..."
    exit 0
fi

DEPENDENCIES_PATH="$(dirname "${0}")/dependencies"

case "${CHOICE}" in
1)
    printf '\n'
    log "= = = = = = = = = ="
    log "Main Packages Installation [Neovim, Emacs, System, ClamAV, UFW, etc] . . ."
    . "${DEPENDENCIES_PATH}/pacman-packages.sh"
    . "${DEPENDENCIES_PATH}/git-packages.sh"
    . "${DEPENDENCIES_PATH}/yay-packages.sh"
    . "${DEPENDENCIES_PATH}/doom-emacs.sh"
    log "💡 Restart [exit/start again] rerun the script with kitty terminal"
    ;;
2)
    printf '\n'
    log "= = = = = = = = = ="
    log "Dev Packages Installation [Docker, Vagrant, K8s, etc.] . . ."
    . "${DEPENDENCIES_PATH}/dev-tools.sh"
    log "💡 Restart [exit/start again] kitty terminal"
    ;;
3)
    printf '\n'
    log "= = = = = = = = = ="
    log "System Drivers & Firmware Installation . . ."
    . "${DEPENDENCIES_PATH}/drivers.sh"
    ;;
*)
    printf '\n'
    log "Invalid CHOICE. Please try again." >&2
    exit 1
    ;;
esac

exit 0
