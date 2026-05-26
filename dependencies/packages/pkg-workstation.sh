#!/usr/bin/env sh
#
# Workstation-specific packages
# Secure coding machine — developer primary desktop
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
    bubblewrap        # low-level sandboxing
    rkhunter          # rootkit detection
    lynis             # system security audit
    veracrypt         # encrypted containers for sensitive files
    aide              # file integrity monitoring

    # -------------------------
    # Password & Secrets Management
    # -------------------------
    keepassxc         # local password manager
    pass              # already in base, explicit here for workstation
    gnupg             # already in base, key signing / encryption

    # -------------------------
    # Hardware 2FA
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
    # Networking / Audit
    # -------------------------
    nmap
    wireshark-qt
    tcpdump
    nethogs
    bandwhich

    # -------------------------
    # Intrusion Detection
    # -------------------------
    fail2ban

    # -------------------------
    # Integrity / Audit
    # -------------------------
    audit
    acct              # process accounting

    # -------------------------
    # Time Sync
    # -------------------------
    chrony

    # -------------------------
    # Multimedia (work-relevant)
    # -------------------------
    obs-studio        # screen recording / demos
    vlc

    # -------------------------
    # Virtualisation (run untrusted code safely)
    # -------------------------
    # libvirt / virt-manager already handled by dev-tools profile
)
