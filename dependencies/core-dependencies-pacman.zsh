#!/usr/bin/env zsh

# Exit on error
set -e
source "$(dirname "$0")/install-utils.zsh"

# -------------------------------------
# Install Common Tools for Arch Linux
# -------------------------------------

echo "🔄 Updating system..."
sudo pacman -Syu --noconfirm

# Define packages
packages=(
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

echo "📦 Installing ${#packages[@]} packages..."
for pkg in "${packages[@]}"; do
    echo -e "\n👉 Installing: \033[1m$pkg\033[0m"
    if ! pacman -Qi "$pkg" &>/dev/null; then
        if sudo pacman -S --needed --noconfirm "$pkg"; then
            echo -e "✅ \033[1m$pkg\033[0m installed."
        else
            echo -e "❌ Failed to install: \033[1m$pkg\033[0m"
        fi
    else
        echo -e "✅ \033[1m$pkg\033[0m is already installed."
    fi
done

# -------------------------------------
# Configure Zsh as Default Shell
# -------------------------------------
if [[ "$SHELL" != *"zsh" ]]; then
    echo "⚙️ Setting Zsh as default shell..."
    chsh -s "$(which zsh)"
else
    echo "✅ Zsh is already the default shell."
fi

# -------------------------------------
# Dotfiles
# -------------------------------------
echo "💾 Copying main config file to home root directory..."
if [[ -d "dotfiles" ]]; then

    backup_and_copy ./dotfiles/.zshrc.d ~/.zshrc.d
    backup_and_copy ~/.zshrc.d/config.d/vim/.vimrc ~/.vimrc
    backup_and_copy ~/.zshrc.d/config.d/kitty ~/.config/kitty
    backup_and_copy ~/.zshrc.d/config.d/env/.env.zsh ~/.env.zsh
    backup_and_copy ~/.zshrc.d/config.d/gitconf/.gitconfig ~/.gitconfig
    backup_and_copy ~/.zshrc.d/config.d/arch/pacman.conf /etc/pacman.conf true

else
    echo "❌ Dotfiles directory not found. Skipping dotfile setup."
fi

# -------------------------------------
# Done
# -------------------------------------
echo "\n🎉 All setup steps completed!"
