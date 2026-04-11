#!/usr/bin/env sh
#
# -------------------------------------
# CLAMAV configuration
# -------------------------------------
#

config_clamav() {

    log "🛡️ Setting up ClamAV..."

    # Stop all existing ClamAV services before reconfiguring
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

    # Allow clamav to send desktop notifications
    if ! grep -q 'clamav ALL' /etc/sudoers.d/clamav 2>/dev/null; then
        echo 'clamav ALL=(ALL) NOPASSWD: SETENV: /usr/bin/notify-send' |
            sudo tee /etc/sudoers.d/clamav >/dev/null
    fi

    log "🔐 Creating ClamAV certs directory with system CA certificates..."
    sudo mkdir -p /etc/clamav/certs

    # Copy the system CA bundle — required by libclamav-rust codesign verifier
    if [ -f /etc/ssl/certs/ca-certificates.crt ]; then
        sudo cp /etc/ssl/certs/ca-certificates.crt /etc/clamav/certs/ca-certificates.crt
        log "✅ System CA bundle copied to /etc/clamav/certs/"
    else
        log "⚠️ /etc/ssl/certs/ca-certificates.crt not found — installing ca-certificates..." >&2
        sudo pacman -S --noconfirm ca-certificates 2>/dev/null || true
        sudo update-ca-trust 2>/dev/null || true
        [ -f /etc/ssl/certs/ca-certificates.crt ] &&
            sudo cp /etc/ssl/certs/ca-certificates.crt /etc/clamav/certs/ca-certificates.crt
    fi

    # Copy individual PEM certs from system store
    if [ -d /etc/ssl/certs ]; then
        sudo find /etc/ssl/certs -maxdepth 1 -name "*.pem" \
            -exec sudo cp {} /etc/clamav/certs/ \;
    fi

    # Run c_rehash to create OpenSSL hashed symlinks (e.g. a3417b12.0)
    # libclamav-rust requires these hashed names to locate certs
    if command -v c_rehash >/dev/null 2>&1; then
        sudo c_rehash /etc/clamav/certs/ 2>/dev/null || true
        log "✅ c_rehash completed — hashed cert symlinks created."
    elif command -v openssl >/dev/null 2>&1; then
        # Fallback: use openssl rehash (available in openssl >= 1.1.1)
        sudo openssl rehash /etc/clamav/certs/ 2>/dev/null || true
        log "✅ openssl rehash completed."
    else
        log "⚠️ Neither c_rehash nor openssl found — certs may not be indexed." >&2
    fi

    sudo chown -R clamav:clamav /etc/clamav/certs
    sudo chmod -R 755 /etc/clamav/certs
    log "✅ ClamAV certs directory ready."

    log "📥 Downloading ClamAV virus database (this may take a few minutes)..."
    if sudo SSL_CERT_DIR=/etc/ssl/certs \
        SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt \
        freshclam --quiet; then
        log "✅ Virus database downloaded successfully."
    else
        log "⚠️ Manual freshclam failed (likely libclamav_rust codesign.rs panic)." >&2
        log "⏳ Falling back to clamav-freshclam.service to download database..." >&2
        sudo systemctl start clamav-freshclam.service

        log "⏳ Waiting for virus database to be downloaded..."
        _db_timeout=300
        while [ ! -f /var/lib/clamav/main.cvd ] && [ ! -f /var/lib/clamav/main.cld ]; do
            sleep 5
            _db_timeout=$((_db_timeout - 5))
            if [ "${_db_timeout}" -le 0 ]; then
                log "❌ Timed out waiting for virus database download." >&2
                log "   Run: sudo journalctl -xeu clamav-freshclam" >&2
                log "   Run: sudo cat /var/log/clamav/freshclam.log" >&2
                return 1
            fi
        done
        log "✅ Virus database downloaded via clamav-freshclam.service."
    fi

    # -----------------
    # Write clamonacc as a SYSTEM unit (requires root/fanotify)
    # -----------------
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
            log "   Run: sudo journalctl -xeu clamav-daemon" >&2
            log "   Run: sudo cat /var/log/clamav/clamd.log" >&2
            return 1
        fi
    done
    log "✅ clamd is ready — socket exists."

    # Start freshclam service for ongoing scheduled updates
    log "🔄 Starting clamav-freshclam service..."
    sudo systemctl enable --now clamav-freshclam.service

    # Now safe to start clamonacc — clamd socket is confirmed ready
    log "🔄 Starting clamav-clamonacc..."
    sudo systemctl enable --now clamav-clamonacc.service

    # Enable scheduled cron tasks
    sudo systemctl enable --now cronie.service

    log "✅ ClamAV setup complete."
    return 0
}
