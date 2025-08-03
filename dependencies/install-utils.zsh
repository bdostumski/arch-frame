#!/usr/bin/env zsh
#
# ----------------------------------------------------------------------
# Installation Utils
# ----------------------------------------------------------------------

export function log() {

    local MESSAGE="${1}"
    local SPECIAL_SYMBOL="${2}"
    local SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    local INSTALLATION_LOG="${3:-${SCRIPT_DIR}/install_messages.log}"

    if [[ ! -f "${INSTALLATION_LOG}" ]]; then
        touch "${INSTALLATION_LOG}"
    fi

    echo -e "${MESSAGE}" "${SPECIAL_SYMBOL}"
    echo -e "$(date) : ${MESSAGE}" "${SPECIAL_SYMBOL}" >> "${INSTALLATION_LOG}"

    return 0
}

export function backup_and_copy() {

    local SRC="${1}"
    local DEST="${2}"
    local IS_ROOT="${3:-false}"

    if [[ $IS_ROOT == "true" ]]; then
        [[ -e "${DEST}" && ! -e "${DEST}.bak" ]] && sudo mv "${DEST}" "${DEST}.bak"
        sudo cp -r "${SRC}" "$DEST"
    else
        [[ -e "${DEST}" && ! -e "${DEST}.bak" ]] && mv "${DEST}" "${DEST}.bak"
        cp -r "${SRC}" "${DEST}"
    fi

    return 0
}

export function install_packman_packages() {

    local PACKAGES="${1}"

    log "📦 Installing ${#PACKAGES[@]} packages..."
    for PKG in "${PACKAGES[@]}"; do
        log "\n👉 Installing: \033[1m${PKG}\033[0m"
        if ! pacman -Qi "${PKG}" &>/dev/null; then
            if sudo pacman -S --needed --noconfirm "${PKG}"; then
                log "✅ \033[1m${PKG}\033[0m installed."
            else
                log "❌ Failed to install: \033[1m${PKG}\033[0m" &>2
            fi
        else
            log "✅ \033[1m${PKG}\033[0m is already installed." &>2
        fi
    done

    return 0
}

export function install_yay_packages() {

    local PACKAGES="${1}"

    log "\n🔧 Starting installation of AUR packages...\n"
    for PKG in "${PACKAGES[@]}"; do
        log "📦 Installing: ${PKG}"
        if yay -Qi "${PKG}" &>/dev/null; then
            log "✅ Already installed: ${PKG}"
        elif yay -S --noconfirm "${PKG}" &>/dev/null; then
            log "✅ Success: ${PKG} installed"
        else
            log "❌ Failed: ${PKG} installation failed"
        fi
        log ""
    done

    return 0
}
