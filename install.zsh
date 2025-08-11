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
log "Install Dependencies 📦"
log "0) Install dependencies neede for the script [Zsh, Kitty, Fonts, Python]"
log "1) Install core dependencies [Vim, Neovim, Emacs, System, ClamAV, UFW, Wine, VirtualBox, etc]"
log "2) Install Dev Tools [Docker, Vagrant, K8s, etc.]"
log "3) Install IDE for programming [Doom Emacs]"
log "4) Install System Drivers & Firmware"
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
0)
    log "\n"
    log "= = = = = = = = = ="
    log "Starting installation [Zsh, Kitty, Fonts, Python] . . ."
    source "${DEPENDENCIES_PATH}/pacman-packages-main.zsh"
    log "💡 Exit default terminal, and run kitty terminal"
    ;;
1)
    log "\n"
    log "= = = = = = = = = ="
    log "Starting installation [Vim, Neovim, Emacs, System, ClamAV, UFW, Wine, VirtualBox, etc] . . ."
    source "${DEPENDENCIES_PATH}/pacman-packages.zsh"
    source "${DEPENDENCIES_PATH}/git-packages.zsh"
    source "${DEPENDENCIES_PATH}/yay-packages.zsh"
    log "💡 Restart [exit/start again] rerun the script with kitty terminal"
    ;;
2)
    log "\n"
    log "= = = = = = = = = ="
    log "Starting installation [Docker, Vagrant, K8s, etc.] . . ."
    source "${DEPENDENCIES_PATH}/dev-tools.zsh"
    log "💡 Restart [exit/start again] kitty terminal"
    ;;

3)
    log "\n"
    log "= = = = = = = = = ="
    log "Starting installation [Doom Emacs] . . ."
    source "${DEPENDENCIES_PATH}/doom-emacs.zsh"
    ;;
4)
    log "\n"
    log "= = = = = = = = = ="
    log "Install System Drivers & Firmware . . ."
    source "${DEPENDENCIES_PATH}/drivers.zsh"
    ;;
*)
    log "\n"
    log -e "Invalid CHOICE. Please try again." ">&2"
    exit 1
    ;;
esac

exit 0
