#!/usr/bin/env zsh
#
# Drivers
# Export packages for drivers.zsh
#

export PACMAN_PACKAGES=(

    # -------------------------
    # Printer Drivers & Tools
    # -------------------------
    cups
    system-config-printer
    foomatic-db
    foomatic-db-engine
    ghostscript

    # -------------------------
    # NVIDIA GPU Drivers
    # -------------------------
    nvidia
    nvidia-utils
    nvidia-settings
    cuda
)

# Note: Brother-specific AUR driver (brother-dcp-l2510d) is installed
# directly inside drivers.zsh to keep it machine-specific.
