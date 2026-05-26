#!/usr/bin/env sh
#
# Security hardening for DESKTOP
#

config_security_desktop() {
    log "🔒 Applying desktop security hardening..."

    # --- sysctl hardening ---
    SYSCTL_CONF="/etc/sysctl.d/99-desktop-security.conf"
    sudo tee "${SYSCTL_CONF}" > /dev/null <<'EOF'
kernel.yama.ptrace_scope = 1
kernel.randomize_va_space = 2
kernel.dmesg_restrict = 1
kernel.sysrq = 0
net.ipv4.tcp_syncookies = 1
net.ipv4.conf.all.rp_filter = 1
net.ipv4.conf.default.rp_filter = 1
net.ipv4.icmp_echo_ignore_broadcasts = 1
net.ipv4.conf.all.log_martians = 1
EOF
    sudo sysctl --system

    # --- UFW rules for desktop ---
    sudo ufw --force reset
    sudo ufw default deny incoming
    sudo ufw default allow outgoing
    sudo ufw allow http
    sudo ufw allow https
    sudo ufw limit 22/tcp
    sudo ufw deny 5900
    sudo ufw logging high
    sudo ufw --force enable

    # --- NVIDIA early KMS ---
    MKINIT_CONF="/etc/mkinitcpio.conf"
    if [ -f "${MKINIT_CONF}" ]; then
        if ! grep -q 'nvidia_modeset' "${MKINIT_CONF}" || ! grep -q 'nvidia_uvm' "${MKINIT_CONF}"; then
            log "⚙️  Adding NVIDIA modules to mkinitcpio..."
            sudo sed -i 's/^MODULES=(\(.*\))/MODULES=(\1 nvidia nvidia_modeset nvidia_uvm nvidia_drm)/' "${MKINIT_CONF}"
            sudo mkinitcpio -P
        fi
    fi

    log "✅ Desktop security hardening complete."
    return 0
}
