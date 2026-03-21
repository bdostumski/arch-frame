#!/usr/bin/env bash
#
# -------------------------------------
# Install COMMON TOOLS for Arch Linux
# -------------------------------------
#

# -------------------------------------
# External IMPORTS
# -------------------------------------
. "$(dirname "${0}")/utils/install-utils.sh"
. "$(dirname "${0}")/packages/pkg-pacman.sh"
. "$(dirname "${0}")/configurations/config-ufw.sh"
. "$(dirname "${0}")/configurations/config-vbox.sh"
. "$(dirname "${0}")/configurations/config-clamav.sh"
. "$(dirname "${0}")/configurations/config-env-variables.sh"
. "$(dirname "${0}")/configurations/config-gitconfig.sh"

log "🔄 Updating system..."

# -------------------------------------
#  Install PACKMAN packages
# -------------------------------------
install_pacman_packages "${PACMAN_PACKAGES[@]}"
# -------------------------------------
# Copy and backup DOTFILES
# -------------------------------------
SCRIPT_DIR="$(cd "$(dirname "${0}")" && pwd)"
DOTFILES="${SCRIPT_DIR}/../dotfiles"
if [ -d "${DOTFILES}" ]; then

    log "💾 Create main config files ..."
    create_env_variables_file
    create_gitconfig_file

    log "💾 Copying main config file to home root directory..."

    CONFIG_DIR="${DOTFILES}/.zshrc.d/config.d"

    backup_and_copy "${DOTFILES}/.zshrc.d" "${HOME}/.zshrc.d" false
    backup_and_copy "${CONFIG_DIR}/vim/.vimrc" "${HOME}/.vimrc" false
    backup_and_copy "${CONFIG_DIR}/kitty" "${HOME}/.config/kitty" false
    backup_and_copy "${CONFIG_DIR}/env/.env.sh" "${HOME}/.env.sh" false
    backup_and_copy "${CONFIG_DIR}/gitconf/.gitconfig" "${HOME}/.gitconfig" false
    backup_and_copy "${CONFIG_DIR}/arch/pacman.conf" "/etc/pacman.conf" true

    if [ -e "${HOME}/.config/nvim" ]; then
        move_file "${HOME}/.config/nvim"
    fi
    backup_and_copy "${DOTFILES}/.zshrc" "${HOME}/.zshrc" false
    backup_and_copy "${CONFIG_DIR}/tmux" "${HOME}/.config/tmux" false
    backup_and_copy "${CONFIG_DIR}/ranger" "${HOME}/.config/ranger" false
    backup_and_copy "${CONFIG_DIR}/clamav" "/etc/clamav" true
    backup_and_copy "${CONFIG_DIR}/cron/cron.daily" "/etc/cron.daily" true
    backup_and_copy "${CONFIG_DIR}/cron/cron.weekly" "/etc/cron.weekly" true
    backup_and_copy "${CONFIG_DIR}/ufw/before.rules" "/etc/ufw/before.rules" true
else

    log "❌ Dotfiles directory not found. Skipping dotfile setup." >&2
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
# NEOVIM kickstart configuration
# -------------------------------------
if [ ! -d "${HOME}/.config/nvim" ]; then
    echo "⚙️  Installing NeoVim kickstart configuration..."
    git clone https://github.com/nvim-lua/kickstart.nvim.git "${HOME}/.config/nvim"
else
    echo "✅ NeoVim configuration already exists at ${HOME}/.config/nvim"
fi

# -------------------------------------
# PYENV configuration
# -------------------------------------
if command -v pyenv >/dev/null 2>&1; then

    log "⚙️ Setting pyenv..."
    pyenv install --latest 3.11
    pyenv global "$(pyenv latest 3.11)"
else

    log "pyenv is not installed!"
fi

chmod +x ~/.zshrc.d/functions.d/*.sh

log "\n🎉 Setup complete. Your system is ready!"
