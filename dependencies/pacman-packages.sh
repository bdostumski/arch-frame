#!/usr/bin/env sh
#
# -------------------------------------
# Install COMMON TOOLS for Arch Linux
# -------------------------------------
#

# -------------------------------------
# Variables 
# -------------------------------------
BASE_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"

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

trap 'stty echo; exit 1' INT TERM

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

    log "💾 Copying main config files to home directory..."

    CONFIG_DIR="${DOTFILES}/.shell.d/config.d"

    # Copy shared shell dir
    backup_and_copy "${DOTFILES}/.shell.d" "${HOME}/.shell.d" false

    # Copy zsh-specific dir
    backup_and_copy "${DOTFILES}/.zshrc.d" "${HOME}/.zshrc.d" false

    # Copy bash-specific dir
    backup_and_copy "${DOTFILES}/.bashrc.d" "${HOME}/.bashrc.d" false

    # Copy shell entry points
    backup_and_copy "${DOTFILES}/.zshrc" "${HOME}/.zshrc" false
    backup_and_copy "${DOTFILES}/.bashrc" "${HOME}/.bashrc" false

    # Copy individual configs
    backup_and_copy "${CONFIG_DIR}/vim/.vimrc" "${HOME}/.vimrc" false
    backup_and_copy "${CONFIG_DIR}/kitty" "${HOME}/.config/kitty" false
    backup_and_copy "${CONFIG_DIR}/tmux" "${HOME}/.config/tmux" false
    backup_and_copy "${CONFIG_DIR}/ranger" "${HOME}/.config/ranger" false
    backup_and_copy "${CONFIG_DIR}/arch/pacman.conf" "/etc/pacman.conf" true
    backup_and_copy "${CONFIG_DIR}/clamav" "/etc/clamav" true
    backup_and_copy "${CONFIG_DIR}/cron/cron.daily" "/etc/cron.daily" true
    backup_and_copy "${CONFIG_DIR}/cron/cron.weekly" "/etc/cron.weekly" true
    backup_and_copy "${CONFIG_DIR}/ufw/before.rules" "/etc/ufw/before.rules" true

    log "💾 Creating main config files..."
    create_env_variables_file
    create_gitconfig_file

    # env and gitconfig are generated — copied from generated location
    backup_and_copy "${HOME}/.shell.d/config.d/env/.env.sh" "${HOME}/.env.sh" false
    backup_and_copy "${HOME}/.shell.d/config.d/gitconf/.gitconfig" "${HOME}/.gitconfig" false

    # NeoVim — move old config first then clone
    move_file "${HOME}/.config/nvim"

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
    if command -v git >/dev/null 2>&1; then
        log "⚙️  Installing NeoVim kickstart configuration..."
        git clone https://github.com/nvim-lua/kickstart.nvim.git "${HOME}/.config/nvim"
    else
        log "❌ git is not installed, skipping NeoVim kickstart setup." >&2
    fi
else
    log "✅ NeoVim configuration already exists at ${HOME}/.config/nvim"
fi

# -------------------------------------
# PYENV configuration
# -------------------------------------
if command -v pyenv >/dev/null 2>&1; then
    log "⚙️ Setting pyenv..."
    PYTHON_VERSION="$(pyenv install --list | grep -e '^\s+3\.[0-9]+\.[0-9]+$' | tail -1 | tr -d ' ')"
    pyenv install "${PYTHON_VERSION}"
    pyenv global "${PYTHON_VERSION}"
else
    log "⚠️ pyenv is not installed, skipping." >&2
fi

# -------------------------------------
# Make functions executable
# -------------------------------------
if [ -d "${HOME}/.shell.d/functions.d" ]; then
    chmod +x "${HOME}/.shell.d/functions.d/"*.sh
fi

printf '\n'
log "🎉 Setup complete. Your system is ready!"
