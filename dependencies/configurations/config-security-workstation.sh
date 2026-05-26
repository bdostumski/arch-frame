#!/usr/bin/env sh
#
# Security hardening for WORKSTATION (secure coding machine)
#

config_security_workstation() {
    log "Applying workstation security hardening..."

    SYSCTL_CONF="/etc/sysctl.d/99-workstation-security.conf"
    sudo tee "${SYSCTL_CONF}" > /dev/null <<'EOF'
kernel.yama.ptrace_scope = 1
kernel.randomize_va_space = 2
kernel.dmesg_restrict = 1
kernel.kptr_restrict = 1
kernel.sysrq = 0
kernel.unprivileged_bpf_disabled = 1
net.core.bpf_jit_harden = 2
net.ipv4.tcp_syncookies = 1
net.ipv4.tcp_rfc1337 = 1
net.ipv4.conf.all.rp_filter = 1
net.ipv4.conf.default.rp_filter = 1
net.ipv4.icmp_echo_ignore_broadcasts = 1
net.ipv4.conf.all.log_martians = 1
net.ipv4.conf.default.log_martians = 1
net.ipv4.conf.all.accept_redirects = 0
net.ipv4.conf.default.accept_redirects = 0
net.ipv4.conf.all.send_redirects = 0
net.ipv6.conf.all.accept_redirects = 0
EOF
    sudo sysctl --system

    sudo ufw --force reset
    sudo ufw default deny incoming
    sudo ufw default allow outgoing
    sudo ufw allow http
    sudo ufw allow https
    sudo ufw limit 22/tcp
    sudo ufw deny 5900
    sudo ufw deny 5800
    sudo ufw logging high
    sudo ufw --force enable

    SSHD_CONF="/etc/ssh/sshd_config"
    if [ -f "${SSHD_CONF}" ]; then
        log "Hardening SSH on workstation..."
        sudo cp "${SSHD_CONF}" "${SSHD_CONF}.bak.$(date +%Y%m%d%H%M%S)"
        sudo sed -i \
            -e 's/^#*PermitRootLogin.*/PermitRootLogin no/' \
            -e 's/^#*PasswordAuthentication.*/PasswordAuthentication no/' \
            -e 's/^#*X11Forwarding.*/X11Forwarding no/' \
            -e 's/^#*MaxAuthTries.*/MaxAuthTries 3/' \
            -e 's/^#*LoginGraceTime.*/LoginGraceTime 20/' \
            "${SSHD_CONF}"
        grep -q '^Protocol ' "${SSHD_CONF}" || echo 'Protocol 2' | sudo tee -a "${SSHD_CONF}" > /dev/null
        sudo systemctl restart sshd
    fi

    if command -v firejail >/dev/null 2>&1; then
        log "Enabling firejail profiles..."
        for app in firefox thunderbird; do
            if command -v "${app}" >/dev/null 2>&1; then
                sudo ln -sf "$(command -v firejail)" "/usr/local/bin/${app}" 2>/dev/null || true
            fi
        done
    fi

    if command -v pcscd >/dev/null 2>&1; then
        sudo systemctl enable --now pcscd.service
    fi

    if command -v fail2ban-server >/dev/null 2>&1; then
        sudo systemctl enable --now fail2ban.service
    fi

    if command -v aide >/dev/null 2>&1; then
        sudo aide --init
        sudo mv /var/lib/aide/aide.db.new /var/lib/aide/aide.db
    fi

    if command -v chronyd >/dev/null 2>&1; then
        sudo systemctl enable --now chronyd.service
    fi

    MKINIT_CONF="/etc/mkinitcpio.conf"
    if [ -f "${MKINIT_CONF}" ]; then
        if ! grep -q 'nvidia_modeset' "${MKINIT_CONF}"; then
            sudo sed -i 's/^MODULES=(\(.*\))/MODULES=(\1 nvidia nvidia_modeset nvidia_uvm nvidia_drm)/' "${MKINIT_CONF}"
            sudo mkinitcpio -P
        fi
    fi

    UMASK_FILE="/etc/profile.d/umask.sh"
    if [ ! -f "${UMASK_FILE}" ]; then
        echo 'umask 027' | sudo tee "${UMASK_FILE}" > /dev/null
    fi

    log "Workstation security hardening complete."
    return 0
}
