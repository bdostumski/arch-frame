#!/usr/bin/env zsh

# Backup and remove existing Neovim config
echo "🔄 Backing up and removing existing Neovim config..."
mv ~/.config/nvim{,.bak}
mv ~/.local/share/nvim{,.bak}
mv ~/.local/state/nvim{,.bak}
mv ~/.cache/nvim{,.bak}

# Clone the LazyVim starter repository
echo "📦 Cloning LazyVim starter repository..."
git clone https://github.com/LazyVim/starter ~/.config/nvim

# Remove the .git directory to avoid conflicts
echo "🗑️ Removing .git directory..."
rm -rf ~/.config/nvim/.git
