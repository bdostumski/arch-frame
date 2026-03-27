#!/usr/bin/env sh
#
# --------------------
# Installation Utils
# --------------------
#

# -----------------------
# System LOG messages
# -----------------------
log() {
    LOG_MESSAGE="${1}"
    LOG_SPECIAL_SYMBOL="${2}"
    LOG_SCRIPT_DIR="$(cd "$(dirname "${0}")" && pwd)"
    LOG_INSTALLATION_LOG="${3:-${LOG_SCRIPT_DIR}/install_messages.log}"

    if [ ! -f "${LOG_INSTALLATION_LOG}" ]; then
        touch "${LOG_INSTALLATION_LOG}"
    fi

    printf '%b\n' "${LOG_MESSAGE} ${LOG_SPECIAL_SYMBOL}"
    printf '%s\n' "$(date "+%F %T") : ${LOG_MESSAGE}" >>"${LOG_INSTALLATION_LOG}"

    return 0
}

# ----------------------------------
# Copy and Backup FILE
# ----------------------------------
backup_and_copy() {
    BAC_SRC="${1}"
    BAC_DEST="${2}"
    BAC_IS_ROOT="${3:-false}"

    if [ "${BAC_IS_ROOT}" = "true" ]; then
        [ -e "${BAC_DEST}" ] && [ ! -e "${BAC_DEST}.bak" ] && sudo mv "${BAC_DEST}" "${BAC_DEST}.bak"
        sudo cp -r "${BAC_SRC}" "${BAC_DEST}"
    else
        [ -e "${BAC_DEST}" ] && [ ! -e "${BAC_DEST}.bak" ] && mv "${BAC_DEST}" "${BAC_DEST}.bak"
        cp -r "${BAC_SRC}" "${BAC_DEST}"
    fi

    return 0
}

# ----------------------------------
# Move FILE to FILE.bak
# ----------------------------------
move_file() {
    MV_SRC="${1}"

    if [ ! -e "${MV_SRC}.bak" ]; then
        log "Moving ${MV_SRC} to ${MV_SRC}.bak"
        mv "${MV_SRC}" "${MV_SRC}.bak"
    else
        log "⚠️ ${MV_SRC}.bak already exists" >&2
        return 1
    fi

    return 0
}

# ----------------------------------
# Install PACMAN packages
# ----------------------------------
install_pacman_packages() {
    log "🔄 PACMAN Updating system..."

    if [ -f "/var/lib/pacman/db.lck" ]; then
        sudo rm "/var/lib/pacman/db.lck"
    fi

    sudo pacman -Syu --noconfirm

    log "📦 Installing ${#} packages..."
    for PAC_PKG in "${@}"; do
        log "📦 Installing: ${PAC_PKG}"
        if ! pacman -Qi "${PAC_PKG}" >/dev/null 2>&1; then
            if sudo pacman -S --needed --noconfirm "${PAC_PKG}"; then
                log "✅ ${PAC_PKG} installed."
            else
                log "❌ Failed to install: ${PAC_PKG}."
            fi
        else
            log "✅ ${PAC_PKG} is already installed."
        fi
    done

    printf '🏁 All packages processed.\n'
    return 0
}

# ----------------------------------
# Install YAY/AUR packages
# ----------------------------------
install_yay_packages() {
    log "🔄 YAY Updating system..."

    if [ -f "/var/lib/pacman/db.lck" ]; then
        sudo rm "/var/lib/pacman/db.lck"
    fi

    sudo chown -R "$(whoami)" ~/.cache/yay
    yay -Syu --noconfirm

    printf '\n'
    log "🔧 Starting installation of AUR packages..."
    printf '\n'
    for YAY_PKG in "${@}"; do
        log "📦 Installing: ${YAY_PKG}"
        if yay -Qi "${YAY_PKG}" >/dev/null 2>&1; then
            log "✅ Already installed: ${YAY_PKG}"
        elif yay -S --noconfirm "${YAY_PKG}" >/dev/null 2>&1; then
            log "✅ Success: ${YAY_PKG} installed"
        else
            log "❌ Failed: ${YAY_PKG} installation failed"
        fi
    done

    printf '🏁 All packages processed.\n'
    return 0
}
