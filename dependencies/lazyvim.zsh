#!/usr/bin/env zsh
#
# -------------------------------------
# Installing LAZIVIM
# -------------------------------------
#

# -------------------------------------
# External Imports
# -------------------------------------
source "$(dirname "${0}")/utils/install-utils.zsh"

# -------------------------------------
# Backup and remove existing NEOVIM config
# -------------------------------------
log "🔄 Backing up and removing existing Neovim config..."
mv ~/.config/nvim{,.bak}
mv ~/.local/share/nvim{,.bak}
mv ~/.local/state/nvim{,.bak}
mv ~/.cache/nvim{,.bak}

# -------------------------------------
# Clone LAZYVIM repository
# -------------------------------------
log "📦 Cloning LazyVim starter repository..."
git clone https://github.com/LazyVim/starter ~/.config/nvim

# -------------------------------------
# Remove .git directory to avoid conflicts
# -------------------------------------
log "🗑️ Removing .git directory..."
rm -rf ~/.config/nvim/.git
