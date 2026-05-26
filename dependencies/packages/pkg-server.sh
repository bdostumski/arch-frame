#!/usr/bin/env sh
# Server-specific packages

export SERVER_PACKAGES=(
    # Intrusion prevention
    fail2ban

    # Integrity checking
    aide

    # Firewall (nftables as an alternative / complement to ufw)
    nftables

    # Monitoring
    prometheus-node-exporter
    lm_sensors
    smartmontools

    # Maintenance
    pacman-contrib   # paccache, checkupdates
    logrotate
    unattended-upgrades

    # Remote access hardening
    openssh          # already in base but explicit here
    libpam-pwquality # password quality (if available)

    # Time sync
    chrony

    # Audit
    audit
    libaudit
)
