#!/usr/bin/env sh
#
# -------------------------------------
# CLAMAV configuration
# -------------------------------------
#

config_clamav() {

    log "🛡️ Setting up ClamAV..."

    # Stop all existing ClamAV services before reconfiguring
    sudo systemctl stop \
        clamav-clamonacc.service \
        clamav-daemon.service \
        clamav-freshclam.service 2>/dev/null || true

    # Clean up any previously copied system CA certs from /etc/clamav/certs/
    # that may lack a CommonName (CN) field, causing codesign.rs:595 panic.
    sudo rm -f /etc/clamav/certs/*.pem /etc/clamav/certs/*.crt 2>/dev/null || true

    # Ensure shadow group exists (needed for clamd to read /etc/shadow)
    if ! getent group shadow >/dev/null 2>&1; then
        sudo groupadd shadow
    fi

    # Ensure clamav system user exists
    if ! id -u clamav >/dev/null 2>&1; then
        sudo useradd -r -s /usr/bin/nologin clamav
    fi

    # Set correct permissions on /etc/shadow
    sudo chown root:shadow /etc/shadow
    sudo chmod 640 /etc/shadow

    # Create all required directories with correct ownership
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

    # Create log files with correct permissions
    sudo touch /var/log/clamav/freshclam.log /var/log/clamav/clamd.log
    sudo chown clamav:clamav \
        /var/log/clamav/freshclam.log \
        /var/log/clamav/clamd.log
    sudo chmod 644 \
        /var/log/clamav/freshclam.log \
        /var/log/clamav/clamd.log

    if [ ! -d /etc/clamav/certs ]; then
        sudo install -d -o clamav -g clamav -m 755 /etc/clamav/certs
    fi
    sudo chown -R clamav:clamav /etc/clamav/certs
    sudo chmod -R 755 /etc/clamav/certs

    # Allow clamav to send desktop notifications
    if ! grep -q 'clamav ALL' /etc/sudoers.d/clamav 2>/dev/null; then
        echo 'clamav ALL=(ALL) NOPASSWD: SETENV: /usr/bin/notify-send' |
            sudo tee /etc/sudoers.d/clamav >/dev/null
    fi

    log "📥 Starting clamav-freshclam.service to download virus database..."
    sudo systemctl start clamav-freshclam.service

    log "⏳ Waiting for virus database download to complete..."
    log "   This may take 2-5 minutes on first install..."
    _db_timeout=300
    while [ ! -f /var/lib/clamav/main.cvd ] &&
        [ ! -f /var/lib/clamav/main.cld ]; do
        sleep 5
        _db_timeout=$((_db_timeout - 5))
        printf '.'
        if [ "${_db_timeout}" -le 0 ]; then
            printf '\n'
            log "❌ Timed out waiting for virus database." >&2
            log "   Check: sudo journalctl -xeu clamav-freshclam" >&2
            log "   Check: sudo cat /var/log/clamav/freshclam.log" >&2
            return 1
        fi
    done
    printf '\n'
    log "✅ Virus database downloaded successfully."

    # Enable freshclam for ongoing scheduled updates
    sudo systemctl enable clamav-freshclam.service

    sudo tee /etc/systemd/system/clamav-clamonacc.service >/dev/null <<'EOF'
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

    # Start clamav-daemon — database now exists so clamd will load successfully
    log "🔄 Starting clamav-daemon..."
    sudo systemctl enable --now clamav-daemon.service

    log "⏳ Waiting for clamd socket to be ready (database load takes ~10-30s)..."
    _timeout=120
    while [ ! -S /run/clamav/clamd.ctl ]; do
        sleep 2
        _timeout=$((_timeout - 2))
        if [ "${_timeout}" -le 0 ]; then
            log "❌ Timed out waiting for clamd socket." >&2
            log "   Check: sudo journalctl -xeu clamav-daemon" >&2
            log "   Check: sudo cat /var/log/clamav/clamd.log" >&2
            return 1
        fi
    done
    log "✅ clamd is ready — socket exists."

    # Start clamonacc — clamd socket is confirmed ready
    log "🔄 Starting clamav-clamonacc..."
    sudo systemctl enable --now clamav-clamonacc.service

    # Enable scheduled cron tasks
    sudo systemctl enable --now cronie.service

    log "✅ ClamAV setup complete."
    return 0
}
