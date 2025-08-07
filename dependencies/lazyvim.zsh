#!/usr/bin/env zsh
#
# -------------------------------------
# Installing LAZIVIM
# -------------------------------------
#

# -------------------------------------
# External IMPORTS
# -------------------------------------
source "$(dirname "${0}")/utils/install-utils.zsh"

# -------------------------------------
# Backup and remove existing NEOVIM config
# -------------------------------------
log "🔄 Backing up and removing existing Neovim config..."

NVIM_DIR="${HOME}/.config/nvim"
move_file "${NVIM_DIR}"

NVIM_SHARE="${HOME}/.local/share/nvim"
move_file "${NVIM_SHARE}"

NVIM_STATE="${HOME}/.local/state/nvim"
move_file "${NVIM_STATE}"

NVIM_CACHE="${HOME}/.cache/nvim"
move_file "${NVIM_CACHE}"

# -------------------------------------
# Clone LAZYVIM repository
# -------------------------------------
log "📦 Cloning LazyVim starter repository..."
git clone "https://github.com/LazyVim/starter" "${HOME}/.config/nvim"

# -------------------------------------
# Remove .GIT directory to avoid conflicts
# -------------------------------------
log "🗑️ Removing .git directory..."
rm -rf "${HOME}/.config/nvim/.git"
