#!/usr/bin/env sh
#
# ─────────────────────────────────────────────────────────────────
# Workstation packages — Secure Coding Machine
# ─────────────────────────────────────────────────────────────────
# Targeted at a developer's primary desktop where security matters.
# Distinct from the gaming-focused "desktop" profile.
#

export WORKSTATION_PACKAGES=(

    # -------------------------
    # NVIDIA (GPU compute / ML)
    # -------------------------
    nvidia
    nvidia-utils
    nvidia-settings
    cuda
    lib32-nvidia-utils

    # -------------------------
    # Security & Sandboxing
    # -------------------------
    firejail          # sandbox for browser and untrusted apps
    bubblewrap        # low-level sandboxing (used by Flatpak too)
    rkhunter          # rootkit detection (complements ClamAV)
    lynis             # full system security audit tool
    veracrypt         # encrypted containers for sensitive project files
    aide              # file integrity monitoring

    # -------------------------
    # Password & Secrets Management
    # -------------------------
    keepassxc         # local password manager
    pass              # CLI password manager (already in base, explicit here)
    gnupg             # key signing / encryption (already in base)

    # -------------------------
    # Hardware 2FA (YubiKey / Smart Card)
    # -------------------------
    yubikey-manager
    pcsclite
    ccid              # smart card daemon

    # -------------------------
    # VPN
    # -------------------------
    wireguard-tools
    openvpn
    networkmanager-openvpn

    # -------------------------
    # Networking / Traffic Audit
    # -------------------------
    nmap
    wireshark-qt
    tcpdump
    nethogs

    # -------------------------
    # Intrusion Prevention
    # -------------------------
    fail2ban

    # -------------------------
    # System Integrity & Audit
    # -------------------------
    audit
    acct              # process accounting

    # -------------------------
    # Accurate Time Sync
    # -------------------------
    chrony

    # -------------------------
    # Multimedia (work-relevant only)
    # -------------------------
    obs-studio        # screen recording / demos / presentations
    vlc
)
