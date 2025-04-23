#!/bin/zsh

# ----------------------------------------
# Install yay (AUR Helper)
# ----------------------------------------
if ! command -v yay &>/dev/null; then
    echo "📦 Installing yay..."
    sudo pacman -S --needed git base-devel --noconfirm
    tmpdir=$(mktemp -d)
    git clone https://aur.archlinux.org/yay.git "$tmpdir/yay"
    cd "$tmpdir/yay" && makepkg -si --noconfirm
    cd ~ && rm -rf "$tmpdir"
else
    echo "✅ yay is already installed."
fi

# ----------------------------------------
# Install Tmux Plugin Manager (TPM)
# ----------------------------------------
TPM_DIR="$HOME/.config/tmux/plugins/tpm"
if [ ! -d "$TPM_DIR" ]; then
    echo "📦 Installing TPM..."
    git clone https://github.com/tmux-plugins/tpm "$TPM_DIR"
else
    echo "✅ TPM already exists at $TPM_DIR"
fi

# ----------------------------------------
# Install Zinit (Zsh Plugin Manager)
# ----------------------------------------
if [ ! -d "$HOME/.config/zinit" ]; then
    echo "📦 Installing Zinit..."
    bash -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma-continuum/zinit/HEAD/scripts/install.sh)"
else
    echo "✅ Zinit already installed."
fi

# ----------------------------------------
# Enable Terraform autocomplete in Zsh
# ----------------------------------------
if ! grep -q 'complete -o nospace -C $(which terraform) terraform' "$HOME/.zshrc"; then
    echo "⚙️ Adding Terraform autocomplete to .zshrc..."
    echo '\n# Terraform autocomplete' >>"$HOME/.zshrc"
    echo 'if command -v terraform &>/dev/null; then' >>"$HOME/.zshrc"
    echo '  complete -o nospace -C $(which terraform) terraform' >>"$HOME/.zshrc"
    echo 'fi' >>"$HOME/.zshrc"
    echo "✅ Terraform autocomplete enabled for Zsh."
else
    echo "✅ Terraform autocomplete already configured in .zshrc"
fi

# ----------------------------------------
# Done
# ----------------------------------------
echo "\n🎉 Script finished successfully!"
