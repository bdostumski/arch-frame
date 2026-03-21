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

    local MESSAGE="${1}"
    local SPECIAL_SYMBOL="${2}"
    local SCRIPT_DIR="$(cd "$(dirname "${0}")" && pwd)"
    local INSTALLATION_LOG="${3:-${SCRIPT_DIR}/install_messages.log}"

    if [ ! -f "${INSTALLATION_LOG}" ]; then
        touch "${INSTALLATION_LOG}"
    fi

    printf '%s\n' "${MESSAGE} ${SPECIAL_SYMBOL}"
    printf '%s\n' "$(date "+%F %T") : ${MESSAGE}" >> "${INSTALLATION_LOG}"

    return 0
}

# ----------------------------------
# Copy and Backup FILE
# ----------------------------------
backup_and_copy() {

    local SRC="${1}"
    local DEST="${2}"
    local IS_ROOT="${3:-false}"

    if [ "${IS_ROOT}" = "true" ]; then
        [ -e "${DEST}" ] && [ ! -e "${DEST}.bak" ] && sudo mv "${DEST}" "${DEST}.bak"
        sudo cp -r "${SRC}" "$DEST"
    else
        [ -e "${DEST}" ] && [ ! -e "${DEST}.bak" ] && mv "${DEST}" "${DEST}.bak"
        cp -r "${SRC}" "${DEST}"
    fi

    return 0
}

# ----------------------------------
# Move FILE to FILE.bak
# ----------------------------------
move_file() {

    local SRC="${1}"

    if [ ! -e "${SRC}.bak" ]; then
        log "Moving ${SRC} to ${SRC}.bak"
        mv "${SRC}" "${SRC}.bak"
    else
        log "⚠️ ${SRC}.bak already exists" >&2
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

    local PACKAGES=("${@}")

    log "📦 Installing ${#PACKAGES[@]} packages..."
    for PKG in "${PACKAGES[@]}"; do
        log "📦 Installing: ${PKG}"
        if ! pacman -Qi "${PKG}" >/dev/null 2>&1; then
            if sudo pacman -S --needed --noconfirm "${PKG}"; then
                log "✅ ${PKG} installed."
            else
                log "❌ Failed to install: ${PKG}."
            fi
        else
            log "✅ ${PKG} is already installed."
        fi
    done

    echo "🏁 All packages processed."
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

    sudo chown -R "${USER}" ~/.cache/yay
    yay -Syu --noconfirm

    local PACKAGES=("${@}")

    log "\n🔧 Starting installation of AUR packages...\n"
    for PKG in "${PACKAGES[@]}"; do
        log "📦 Installing: ${PKG}"
        if yay -Qi "${PKG}" >/dev/null 2>&1; then
            log "✅  Already installed: ${PKG}"
        elif yay -S --noconfirm "${PKG}" >/dev/null 2>&1; then
            log "✅ Success: ${PKG} installed"
        else
            log "❌ Failed: ${PKG} installation failed"
        fi
    done

    echo "🏁 All packages processed."
    return 0
}
