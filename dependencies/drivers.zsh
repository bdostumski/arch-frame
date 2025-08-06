#!/usr/bin/env zsh
#
# ----------------------------------------------------------------------
# Install Drivers
# ----------------------------------------------------------------------

# -------------------------------------
# External Imports
# -------------------------------------
source "$(dirname "${0}")/utils/install-utils.zsh"
source "$(dirname "${0}")/packages/pkg-drivers.zsh"

# -------------------------------------
#  Install PACMAN Packages
# -------------------------------------
install_packman_packages "${PACMAN_PACKAGES[@]}"

# -------------------------------------
# PRINTER configuration
# -------------------------------------
log "✅ Enable cups"
sudo systemctl enable --now cups.service

# -------------------------------------
#  Install AUR Packages
# -------------------------------------
AUR_PACKAGES=(brother-dcp-l2510d)
install_yay_packages "${AUR_PACKAGES[@]}"

log "✅ Restart cups"
sudo systemctl restart cups.service

# -------------------------------------
# Nvidia configuration
# -------------------------------------
log "✅ Setup Nvidia"
log 'export PATH=/opt/cuda/bin:$PATH' >>~/.zshrc.d/environment.zsh
log 'export LD_LIBRARY_PATH=/opt/cuda/lib64:$LD_LIBRARY_PATH' >>~/.zshrc.d/environment.zsh
source ~/.zshrc

# -------------------------------------
# System setup
# Enable the Out-of-Memory daemon for better memory management
# -------------------------------------
sudo systemctl enable systemd-oomd --now

# -------------------------------------
# DONE
# -------------------------------------
log "\n🎉 All setup steps completed!"
