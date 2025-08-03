#!/usr/bin/env zsh
#
# ----------------------------------------
# Install YAY (AUR Helper)
# ----------------------------------------

if ! command -v yay &>/dev/null; then
    echo "📦 Installing yay..."
    sudo pacman -S --needed git base-devel --noconfirm
    TMPDIR="$(mktemp -d)"
    git clone https://aur.archlinux.org/yay.git "${TMPDIR}/yay"
    cd "${TMPDIR}/yay" && makepkg -si --noconfirm
    cd ~ && rm -rf "${TMPDIR}"
else
    echo "✅ yay is already installed." >&2
fi

# ----------------------------------------
# Install Tmux Plugin Manager (TPM)
# ----------------------------------------
TPM_DIR="${HOME}/.config/tmux/plugins/tpm"
if [[ ! -d "${TPM_DIR}" ]]; then
    echo "📦 Installing TPM..."
    git clone https://github.com/tmux-plugins/tpm "${TPM_DIR}"
else
    echo "✅ TPM already exists at $TPM_DIR" >&2
fi

# ----------------------------------------
# Install Zinit (Zsh Plugin Manager)
# ----------------------------------------
if [[ ! -d "${HOME}/.config/zinit" ]]; then
    echo "📦 Installing Zinit..."
    bash -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma-continuum/zinit/HEAD/scripts/install.sh)"
else
    echo "✅ Zinit already installed." >&2
fi

# ----------------------------------------
# Done
# ----------------------------------------
echo "\n🎉 Script finished successfully!"
