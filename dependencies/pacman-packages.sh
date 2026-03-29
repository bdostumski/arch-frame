#!/usr/bin/env sh
#
# -------------------------------------
# Install COMMON TOOLS for Arch Linux
# -------------------------------------
#

# -------------------------------------
# Variables 
# -------------------------------------
BASE_DIR="$(cd "$(dirname "${0}")" && pwd)"

# -------------------------------------
# External IMPORTS
# -------------------------------------
. "${BASE_DIR}/dependencies/utils/install-utils.sh"
. "${BASE_DIR}/dependencies/packages/pkg-pacman.sh"
. "${BASE_DIR}/dependencies/configurations/config-ufw.sh"
. "${BASE_DIR}/dependencies/configurations/config-vbox.sh"
. "${BASE_DIR}/dependencies/configurations/config-clamav.sh"
. "${BASE_DIR}/dependencies/configurations/config-env-variables.sh"
. "${BASE_DIR}/dependencies/configurations/config-gitconfig.sh"

log "🔄 Updating system..."
sudo pacman -Syu --noconfirm

# -------------------------------------
#  Install PACKMAN packages
# -------------------------------------
install_pacman_packages "${PACMAN_PACKAGES[@]}"
# -------------------------------------
# Copy and backup DOTFILES
# -------------------------------------
DOTFILES="${BASE_DIR}/dotfiles"
if [ -d "${DOTFILES}" ]; then

    log "💾 Create main config files ..."
    create_env_variables_file
    create_gitconfig_file

    log "💾 Copying main config file to home root directory..."

    CONFIG_DIR="${DOTFILES}/.shell.d/config.d"

    backup_and_copy "${DOTFILES}/.zshrc.d" "${HOME}/.zshrc.d" false
    backup_and_copy "${CONFIG_DIR}/vim/.vimrc" "${HOME}/.vimrc" false
    backup_and_copy "${CONFIG_DIR}/kitty" "${HOME}/.config/kitty" false
    backup_and_copy "${CONFIG_DIR}/env/.env.sh" "${HOME}/.env.sh" false
    backup_and_copy "${CONFIG_DIR}/gitconf/.gitconfig" "${HOME}/.gitconfig" false
    backup_and_copy "${CONFIG_DIR}/arch/pacman.conf" "/etc/pacman.conf" true

    move_file "${HOME}/.config/nvim"
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
    log "⚙️  Installing NeoVim kickstart configuration..."
    git clone https://github.com/nvim-lua/kickstart.nvim.git "${HOME}/.config/nvim"
else
    log "✅ NeoVim configuration already exists at ${HOME}/.config/nvim"
fi

# -------------------------------------
# PYENV configuration
# -------------------------------------
if command -v pyenv >/dev/null 2>&1; then

    log "⚙️ Setting pyenv..."
    pyenv install 3.11.6
    pyenv global 3.11.6
else

    log "pyenv is not installed!"
fi

chmod +x ~/.zshrc.d/functions.d/*.sh

printf '\n'
log "🎉 Setup complete. Your system is ready!"
