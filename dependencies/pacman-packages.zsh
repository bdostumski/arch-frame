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
source "$(dirname "${0}")/packages/pkg-pacman.zsh"
source "$(dirname "${0}")/configurations/config-ufw.zsh"
source "$(dirname "${0}")/configurations/config-vbox.zsh"

log "🔄 Updating system..."
sudo pacman -Syu --noconfirm

# -------------------------------------
#  Install PACKMAN packages
# -------------------------------------
install_packman_packages "${PACMAN_PACKAGES[@]}"

# -------------------------------------
# Copy and backup DOTFILES
# -------------------------------------
DOTFILES="../dotfiles"
log "💾 Copying main config file to home root directory..."
if [[ -d "${DOTFILES}" ]]; then

    CONFIG_DIR="${HOME}/.zshrc.d/config.d"

    backup_and_copy "${DOTFILES}/.zshrc" "${HOME}/.zshrc" false
    backup_and_copy "${CONFIG_DIR}/nvim" "${HOME}/.config/nvim" false
    backup_and_copy "${CONFIG_DIR}/tmux" "${HOME}/.config/tmux" false
    backup_and_copy "${CONFIG_DIR}/ranger" "${HOME}/.config/ranger" false
    backup_and_copy "${CONFIG_DIR}/clamav" "/etc/clamav" true
    backup_and_copy "${CONFIG_DIR}/cron/cron.daily" "/etc/cron.daily" true
    backup_and_copy "${CONFIG_DIR}/cron/cron.weekly" "/etc/cron.weekly" true
    backup_and_copy "${CONFIG_DIR}/ufw/before.rules" "/etc/ufw/before.rules" true
else

    log "❌ Dotfiles directory not found. Skipping dotfile setup." "&>2"
fi

# -------------------------------------
# VBOX DRIVERS (only if using VirtualBox with Vagrant)
# -------------------------------------
config_vbox

# -------------------------------------
# UFW FIREWALL configuration
# -------------------------------------
config_ufw

# -------------------------------------
# CLAMAV configuration
# -------------------------------------
config_clamav

# -------------------------------------
# ZSH configuration
# -------------------------------------
if [[ "${SHELL}" != *"zsh" ]]; then

    log "⚙️ Setting Zsh as default shell..."
    chsh -s "$(which zsh)"
else

    log "✅ Zsh is already the default shell." >&2
fi

chmod +x ~/.zshrc.d/functions.d/*.zsh

log "\n🎉 Setup complete. Your system is ready!"
