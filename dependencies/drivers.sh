#!/usr/bin/env sh
#
# -------------------------------------
# Install DRIVERS
# -------------------------------------
#

# -------------------------------------
# External IMPORTS
# -------------------------------------
. "$(dirname "${0}")/utils/install-utils.sh"
. "$(dirname "${0}")/packages/pkg-drivers.sh"

# -------------------------------------
#  Install PACMAN Packages
# -------------------------------------
install_packman_packages "${PACMAN_PACKAGES[@]}"

# -------------------------------------
#  Install AUR Packages
# -------------------------------------
AUR_PACKAGES=(brother-dcp-l2510d)
install_yay_packages "${AUR_PACKAGES[@]}"

# -------------------------------------
# PRINTER configuration
# -------------------------------------
log "✅ Enable cups"
sudo systemctl enable --now cups.service

log "✅ Restart cups"
sudo systemctl restart cups.service

# -------------------------------------
# NVIDIA configuration
# -------------------------------------
log "✅ Setup Nvidia"
printf 'export PATH=/opt/cuda/bin:$PATH\n' >> ~/.profile
printf 'export LD_LIBRARY_PATH=/opt/cuda/lib64:$LD_LIBRARY_PATH\n' >> ~/.profile
. "${HOME}/.profile"

# -------------------------------------
# System setup
# Enable the Out-of-Memory daemon for better memory management
# -------------------------------------
sudo systemctl enable systemd-oomd --now

# -------------------------------------
# DONE
# -------------------------------------
log "\n🎉 All setup steps completed!"
