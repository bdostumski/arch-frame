#!/usr/bin/env zsh
#
# -------------------------------------
# Install COMMON TOOLS for Arch Linux
# -------------------------------------
#

# -------------------------------------
# External IMPORTS
# -------------------------------------
source "$(dirname "${0}")/utils/install-utils.zsh"
source "$(dirname "${0}")/packages/pkg-pacman-main.zsh"

# -------------------------------------
#  Install PACKMAN packages
# -------------------------------------
install_packman_packages "${PACMAN_PACKAGES[@]}"

# -------------------------------------
# Configure ZSH as default shell
# -------------------------------------
if [[ "${SHELL}" != *"zsh" ]]; then

    log "⚙️ Setting Zsh as default shell..."
    chsh -s "$(which zsh)"
else

    log "✅ Zsh is already the default shell." ">&2"
fi

# -------------------------------------
# Copy and backup DOTFILES
# -------------------------------------
DOTFILES="dotfiles"
log "💾 Copying main config file to home root directory..."
if [[ -d "${DOTFILES}" ]]; then

    CONFIG_DIR="${HOME}/.zshrc.d/config.d"

    backup_and_copy "${DOTFILES}/.zshrc.d" "${HOME}/.zshrc.d" false
    backup_and_copy "${CONFIG_DIR}/vim/.vimrc" "${HOME}/.vimrc" false
    backup_and_copy "${CONFIG_DIR}/kitty" "${HOME}/.config/kitty" false
    backup_and_copy "${CONFIG_DIR}/env/.env.zsh" "${HOME}/.env.zsh" false
    backup_and_copy "${CONFIG_DIR}/gitconf/.gitconfig" "${HOME}/.gitconfig" false
    backup_and_copy "${CONFIG_DIR}/arch/pacman.conf" "/etc/pacman.conf" true
else

    log "❌ Dotfiles directory not found. Skipping dotfile setup." ">&2"
    return 1
fi

# -------------------------------------
# DONE
# -------------------------------------
log "\n🎉 All setup steps completed!"
