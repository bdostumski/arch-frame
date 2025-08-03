#!/usr/bin/env zsh
#
# ----------------------------------------------------------------------
# Install Core Dependencies
# ----------------------------------------------------------------------

# Exit on error
set -e

# Import Install Utils
source "$(dirname "${0}")/install-utils.zsh"

# -------------------------------------
# Install Common Tools for Arch Linux
# -------------------------------------

log "🔄 Updating system..."
sudo pacman -Syu --noconfirm

# Define packages
PACMAN_PACKAGES=(

    # Linux kernel
    linux-zen linux-zen-headers

    # Fonts
    kitty ttf-dejavu ttf-liberation ttf-roboto ttf-ubuntu-font-family noto-fonts
    noto-fonts-emoji noto-fonts-cjk ttf-fira-code ttf-fira-mono ttf-fira-sans
    ttf-jetbrains-mono ttf-hack ttf-inconsolata nerd-fonts ttf-opensans

    # Terminal Fonts
    terminus-font

    # System Utilities
    zsh git github-cli ranger

    # Python + tools
    python python-pip python-pipenv python-virtualenv python-pynvim pyenv
)

# -------------------------------------
#  Install Packman Packages
# -------------------------------------
install_packman_packages "${PACMAN_PACKAGES}"

# -------------------------------------
# Configure Zsh as Default Shell
# -------------------------------------
if [[ "${SHELL}" != *"zsh" ]]; then
    log "⚙️ Setting Zsh as default shell..."
    chsh -s "$(which zsh)"
else
    log "✅ Zsh is already the default shell." >&2
fi

# -------------------------------------
# Dotfiles
# -------------------------------------
DOTFILES="dotfiles"
log "💾 Copying main config file to home root directory..."
if [[ -d "${DOTFILES}" ]]; then

    local CONFIG_DIR="${HOME}/.zshrc.d/config.d"

    backup_and_copy "${DOTFILES}/.zshrc.d" "${HOME}/.zshrc.d" false
    backup_and_copy "${CONFIG_DIR}/vim/.vimrc" "${HOME}/.vimrc" false
    backup_and_copy "${CONFIG_DIR}/kitty" "${HOME}/.config/kitty" false
    backup_and_copy "${CONFIG_DIR}/env/.env.zsh" "${HOME}/.env.zsh" false
    backup_and_copy "${CONFIG_DIR}/gitconf/.gitconfig" "${HOME}/.gitconfig" false
    backup_and_copy "${CONFIG_DIR}/arch/pacman.conf" "/etc/pacman.conf" true

else
    log "❌ Dotfiles directory not found. Skipping dotfile setup." >&2
    return 1
fi

# -------------------------------------
# Done
# -------------------------------------
log "\n🎉 All setup steps completed!"
