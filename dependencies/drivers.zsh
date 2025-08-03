#!/usr/bin/env zsh
#
# ----------------------------------------------------------------------
# Install Drivers
# ----------------------------------------------------------------------

# Import Install Utils
source "$(dirname "${0}")/install-utils.zsh"

# Define packages
PACMAN_PACKAGES=(

    # Printer
    cups system-config-printer foomatic-db foomatic-db-engine ghostscript

    # Nvidia
    nvidia nvidia-utils nvidia-settings cuda

)

# -------------------------------------
#  Install Pacman Packages
# -------------------------------------
install_packman_packages "${PACMAN_PACKAGES}"

# Setup printer
echo "✅ Enable cups"
sudo systemctl enable --now cups.service

# -------------------------------------
#  Install AUR Packages
# -------------------------------------
AUR_PACKAGES=(brother-dcp-l2510d)
install_yay_packages "${AUR_PACKAGES}"

echo "✅ Restart cups"
sudo systemctl restart cups.service

# Setup Nvidia
echo "✅ Setup Nvidia"
echo 'export PATH=/opt/cuda/bin:$PATH' >>~/.zshrc.d/environment.zsh
echo 'export LD_LIBRARY_PATH=/opt/cuda/lib64:$LD_LIBRARY_PATH' >>~/.zshrc.d/environment.zsh
source ~/.zshrc

# System setup
sudo systemctl enable systemd-oomd --now # Enable the Out-of-Memory daemon for better memory management:
