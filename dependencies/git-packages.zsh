#!/usr/bin/env zsh
#
# ----------------------------------------
# Install YAY (AUR Helper)
# ----------------------------------------
#

# ----------------------------------------
# External IMPORTS
# ----------------------------------------
source "$(dirname "${0}")/utils/install-utils.zsh"

if ! command -v yay &>/dev/null; then

    log "📦 Installing yay..."

    sudo pacman -S --needed git base-devel --noconfirm
    TMPDIR="$(mktemp -d)"
    git clone https://aur.archlinux.org/yay.git "${TMPDIR}/yay"
    cd "${TMPDIR}/yay" && makepkg -si --noconfirm
    cd ~ && rm -rf "${TMPDIR}"
else
    log "✅ yay is already installed." ">&2"
fi

# ----------------------------------------
# Install TMUX plugin manager (TPM)
# ----------------------------------------
TPM_DIR="${HOME}/.config/tmux/plugins/tpm"
if [[ ! -d "${TPM_DIR}" ]]; then
    log "📦 Installing TPM..."
    git clone https://github.com/tmux-plugins/tpm "${TPM_DIR}"
else

    log "✅ TPM already exists at $TPM_DIR" ">&2"
fi

# ----------------------------------------
# Install ZINIT - ZSH plugin manager
# ----------------------------------------
if [[ ! -d "${HOME}/.config/zinit" ]]; then
    log "📦 Installing Zinit..."
    bash -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma-continuum/zinit/HEAD/scripts/install.sh)"
else
    log "✅ Zinit already installed." ">&2"
fi

# ----------------------------------------
# DONE
# ----------------------------------------
log "\n🎉 Script finished successfully!"
