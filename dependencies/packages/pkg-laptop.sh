#!/usr/bin/env sh
# Laptop-specific packages

export LAPTOP_PACKAGES=(
    # Power management
    tlp
    tlp-rdw
    powertop
    acpid
    auto-cpufreq

    # Backlight
    brightnessctl
    light

    # Bluetooth
    bluez
    bluez-utils

    # Wireless
    iw
    wireless_tools
    wpa_supplicant
    networkmanager
    network-manager-applet

    # Full-disk encryption helpers
    cryptsetup

    # Touchpad
    libinput
    xf86-input-libinput

    # Screen lock
    swaylock
    xautolock

    # Battery info
    acpi

    # Suspend / hibernate
    pm-utils
)
