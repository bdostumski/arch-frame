#!/usr/bin/env sh
#
# Security hardening for LAPTOP
#

config_security_laptop() {
    log "🔒 Applying laptop security hardening..."

    # --- sysctl hardening ---
    SYSCTL_CONF="/etc/sysctl.d/99-laptop-security.conf"
    sudo tee "${SYSCTL_CONF}" > /dev/null <<'EOF'
# Disable ptrace for non-children processes
kernel.yama.ptrace_scope = 1

# Enable ASLR
kernel.randomize_va_space = 2

# Restrict dmesg to root
kernel.dmesg_restrict = 1

# Disable magic SysRq
kernel.sysrq = 0

# Disable IPv6 if not needed (comment out if you use IPv6)
# net.ipv6.conf.all.disable_ipv6 = 1

# TCP hardening
net.ipv4.tcp_syncookies = 1
net.ipv4.conf.all.rp_filter = 1
net.ipv4.conf.default.rp_filter = 1
net.ipv4.icmp_echo_ignore_broadcasts = 1
net.ipv4.conf.all.log_martians = 1
EOF
    sudo sysctl --system

    # --- UFW rules for laptop ---
    sudo ufw --force reset
    sudo ufw default deny incoming
    sudo ufw default allow outgoing
    sudo ufw allow http
    sudo ufw allow https
    sudo ufw limit 22/tcp
    sudo ufw deny 5900        # VNC
    sudo ufw logging high
    sudo ufw --force enable

    # --- TLP power + security ---
    if command -v tlp >/dev/null 2>&1; then
        sudo systemctl enable --now tlp.service
    fi

    # --- Screen lock on suspend ---
    if command -v xautolock >/dev/null 2>&1; then
        log "💡 xautolock installed — add it to your autostart to lock on idle."
    fi

    log "✅ Laptop security hardening complete."
    return 0
}
