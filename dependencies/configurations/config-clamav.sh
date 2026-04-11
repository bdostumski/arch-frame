#!/usr/bin/env sh
#
# -------------------------------------
# CLAMAV configuration
# -------------------------------------
#

config_clamav() {

    log "🛡️ Setting up ClamAV..."

    # Stop existing CLAMAV services before reconfiguring
    sudo systemctl stop clamav-clamonacc.service clamav-daemon.service clamav-freshclam.service 2>/dev/null || true

    # Ensure shadow group exists (needed for clamd to read /etc/shadow)
    if ! getent group shadow >/dev/null 2>&1; then
        sudo groupadd shadow
    fi

    # Ensure clamav system user exists
    if ! id -u clamav >/dev/null 2>&1; then
        sudo useradd -r -s /usr/bin/nologin clamav
    fi

    # Set correct permissions on /etc/shadow
    sudo chown root:shadow /etc/shadow && sudo chmod 640 /etc/shadow

    # Create required directories with correct ownership
    sudo install -d -o clamav -g clamav -m 755 \
        /var/lib/clamav \
        /var/log/clamav \
        /var/run/clamav \
        /root/quarantine

    # Create user quarantine directory
    if [ ! -d "${HOME}/quarantine" ]; then
        mkdir -p "${HOME}/quarantine"
    fi
    sudo chown -R clamav:clamav "${HOME}/quarantine"

    # Create freshclam log file with correct permissions
    sudo touch /var/log/clamav/freshclam.log
    sudo chown clamav:clamav /var/log/clamav/freshclam.log
    sudo chmod 644 /var/log/clamav/freshclam.log

    # Allow clamav to send desktop notifications via sudo
    if ! grep -q 'clamav ALL' /etc/sudoers.d/clamav 2>/dev/null; then
        echo 'clamav ALL=(ALL) NOPASSWD: SETENV: /usr/bin/notify-send' | sudo tee /etc/sudoers.d/clamav >/dev/null
    fi

    sudo tee /etc/systemd/system/clamav-clamonacc.service >/dev/null <<EOF
[Unit]
Description=ClamAV On-Access Scanner
Documentation=man:clamonacc(8)
After=clamav-daemon.service
Requires=clamav-daemon.service

[Service]
Type=simple
ExecStartPre=/bin/sh -c 'until [ -S /run/clamav/clamd.ctl ]; do sleep 1; done'
ExecStart=/usr/bin/clamonacc -F --fdpass --log=/var/log/clamav/clamonacc.log
Restart=on-failure
RestartSec=10s
TimeoutStartSec=120

[Install]
WantedBy=multi-user.target
EOF

    sudo systemctl daemon-reload

    log "🔄 Starting clamav-daemon and waiting for it to be ready..."
    sudo systemctl enable --now clamav-daemon.service

    # Wait for clamd socket — database load can take up to 30 seconds
    log "⏳ Waiting for clamd socket to be ready..."
    _timeout=60
    while [ ! -S /run/clamav/clamd.ctl ]; do
        sleep 2
        _timeout=$((_timeout - 2))
        if [ "${_timeout}" -le 0 ]; then
            log "❌ Timed out waiting for clamd socket. Check: journalctl -xeu clamav-daemon" >&2
            return 1
        fi
    done
    log "✅ clamd is ready."

    log "🔄 Starting clamav-freshclam.service..."
    sudo systemctl enable --now clamav-freshclam.service

    # Now it is safe to start clamonacc — clamd socket is confirmed ready
    log "🔄 Starting clamav-clamonacc.service..."
    sudo systemctl enable --now clamav-clamonacc.service

    # Enable scheduled tasks (cron)
    sudo systemctl enable --now cronie.service

    log "✅ ClamAV setup complete."
    return 0
}
