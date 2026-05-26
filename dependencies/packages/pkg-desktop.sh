#!/usr/bin/env sh
# Desktop-specific packages

export DESKTOP_PACKAGES=(
    # NVIDIA
    nvidia
    nvidia-utils
    nvidia-settings
    cuda
    lib32-nvidia-utils

    # Gaming
    steam
    gamemode
    lib32-gamemode
    mangohud

    # Wine / Compatibility
    wine
    wine-mono
    wine-gecko
    winetricks

    # Multimedia
    obs-studio
    kdenlive
    vlc

    # Display / Compositor
    picom

    # Peripherals
    piper          # gaming mouse config
    solaar         # Logitech unifying receiver
)
