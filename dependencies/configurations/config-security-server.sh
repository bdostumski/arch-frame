#!/usr/bin/env sh
#
# Security hardening for SERVER
#

config_security_server() {
    log "🔒 Applying server security hardening..."

    # --- sysctl hardening (strict) ---
    SYSCTL_CONF="/etc/sysctl.d/99-server-security.conf"
    sudo tee "${SYSCTL_CONF}" > /dev/null <<'EOF'
# Kernel hardening
kernel.yama.ptrace_scope = 2
kernel.randomize_va_space = 2
kernel.dmesg_restrict = 1
kernel.kptr_restrict = 2
kernel.sysrq = 0
kernel.unprivileged_bpf_disabled = 1
net.core.bpf_jit_harden = 2

# TCP hardening
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
net.ipv6.conf.default.accept_redirects = 0
EOF
    sudo sysctl --system

    # --- Strict UFW for server ---
    sudo ufw --force reset
    sudo ufw default deny incoming
    sudo ufw default deny outgoing      # deny all outgoing — only allow what is needed
    sudo ufw allow out 80/tcp
    sudo ufw allow out 443/tcp
    sudo ufw allow out 53
    sudo ufw allow out 123/udp          # NTP
    sudo ufw allow in 80/tcp
    sudo ufw allow in 443/tcp
    sudo ufw limit in 22/tcp
    sudo ufw logging high
    sudo ufw --force enable

    # --- SSH hardening ---
    SSHD_CONF="/etc/ssh/sshd_config"
    if [ -f "${SSHD_CONF}" ]; then
        log "🔧 Hardening SSH..."
        sudo cp "${SSHD_CONF}" "${SSHD_CONF}.bak.$(date +%Y%m%d%H%M%S)"
        sudo sed -i \
            -e 's/^#*PermitRootLogin.*/PermitRootLogin no/' \
            -e 's/^#*PasswordAuthentication.*/PasswordAuthentication no/' \
            -e 's/^#*X11Forwarding.*/X11Forwarding no/' \
            -e 's/^#*MaxAuthTries.*/MaxAuthTries 3/' \
            -e 's/^#*LoginGraceTime.*/LoginGraceTime 20/' \
            -e 's/^#*AllowAgentForwarding.*/AllowAgentForwarding no/' \
            -e 's/^#*AllowTcpForwarding.*/AllowTcpForwarding no/' \
            -e 's/^#*PrintMotd.*/PrintMotd no/' \
            "${SSHD_CONF}"
        grep -q '^ClientAliveInterval ' "${SSHD_CONF}" || echo 'ClientAliveInterval 300' | sudo tee -a "${SSHD_CONF}" > /dev/null
        grep -q '^ClientAliveCountMax ' "${SSHD_CONF}" || echo 'ClientAliveCountMax 2' | sudo tee -a "${SSHD_CONF}" > /dev/null
        sudo systemctl restart sshd
    fi

    # --- fail2ban ---
    if command -v fail2ban-server >/dev/null 2>&1; then
        log "🔧 Enabling fail2ban..."
        sudo systemctl enable --now fail2ban.service
    fi

    # --- AIDE integrity database ---
    if command -v aide >/dev/null 2>&1; then
        log "🔧 Initialising AIDE integrity database (this may take a while)..."
        if sudo aide --init; then
            sudo mv /var/lib/aide/aide.db.new /var/lib/aide/aide.db
            log "💡 Schedule 'aide --check' in a cron job to detect tampering."
        else
            log "❌ AIDE initialization failed. Run 'sudo aide --init' manually after install." >&2
        fi
    fi

    # --- chrony time sync ---
    if command -v chronyd >/dev/null 2>&1; then
        sudo systemctl enable --now chronyd.service
    fi

    log "✅ Server security hardening complete."
    return 0
}
