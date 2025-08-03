#!/usr/bin/env zsh
#
# ----------------------------------------------------------------------
# Installation Utils
# ----------------------------------------------------------------------

export function log() {

    local MESSAGE="${1}"
    local SPECIAL_SYMBOL="${2}"
    local INSTALLATION_LOG="${3:-../installation_log_message}"

    if [[ -f "${INSTALLATION_LOG}" ]]; then
        touch "${INSTALLATION_LOG}"
    fi

    echo -e "${MESSAGE}" "${SPECIAL_SYMBOL}"
    echo -e "$(date) : ${MESSAGE}" "${SPECIAL_SYMBOL}" >> "${INSTALATION_LOG}"

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

    echo "📦 Installing ${#PACKAGES[@]} packages..."
    for PKG in "${PACKAGES[@]}"; do
        echo -e "\n👉 Installing: \033[1m${PKG}\033[0m"
        if ! pacman -Qi "${PKG}" &>/dev/null; then
            if sudo pacman -S --needed --noconfirm "${PKG}"; then
                echo -e "✅ \033[1m${PKG}\033[0m installed."
            else
                echo -e "❌ Failed to install: \033[1m${PKG}\033[0m" &>2
            fi
        else
            echo -e "✅ \033[1m${PKG}\033[0m is already installed." &>2
        fi
    done

    return 0
}

export function install_yay_packages() {

    local PACKAGES="${1}"

    echo "\n🔧 Starting installation of AUR packages...\n"
    for PKG in "${PACKAGES[@]}"; do
        echo "📦 Installing: ${PKG}"
        if yay -Qi "${PKG}" &>/dev/null; then
            echo "✅ Already installed: ${PKG}"
        elif yay -S --noconfirm "${PKG}" &>/dev/null; then
            echo "✅ Success: ${PKG} installed"
        else
            echo "❌ Failed: ${PKG} installation failed"
        fi
        echo ""
    done

    return 0
}
