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
. "$(dirname "${0}")/install-config.sh"

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

# -------------------------------------
# Set default login shell
# -------------------------------------
if [ -n "${DEFAULT_SHELL}" ]; then
    # Map config value -> shell path
    case "${DEFAULT_SHELL}" in
    zsh) TARGET_SHELL="$(command -v zsh 2>/dev/null)" ;;
    bash) TARGET_SHELL="$(command -v bash 2>/dev/null)" ;;
    fish) TARGET_SHELL="$(command -v fish 2>/dev/null)" ;;
    *)
        log "⚠️ DEFAULT_SHELL='${DEFAULT_SHELL}' is not supported. Skipping shell change."
        TARGET_SHELL=""
        ;;
    esac

    if [ -n "${TARGET_SHELL}" ]; then
        if command -v chsh >/dev/null 2>&1; then
            # If already default, do nothing
            if [ "${SHELL}" = "${TARGET_SHELL}" ]; then
                log "✅ Default shell already set to ${TARGET_SHELL}"
            else
                log "🔧 Setting default shell for ${USER_NAME} to ${TARGET_SHELL}"
                # chsh may prompt for password
                if chsh -s "${TARGET_SHELL}" "${USER_NAME}"; then
                    log "✅ Default shell changed to ${TARGET_SHELL}. Log out/in to apply."
                else
                    log "❌ Failed to change default shell (chsh returned non-zero)."
                fi
            fi
        else
            log "⚠️ chsh not found. Skipping default shell change."
        fi
    else
        log "⚠️ Shell '${DEFAULT_SHELL}' not installed/found in PATH. Skipping."
    fi
fi

exit 0
