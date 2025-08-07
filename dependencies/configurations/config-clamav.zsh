#!/usr/bin/env zsh
#
# -------------------------------------
# CLAMAV configuration
# -------------------------------------
#

# -------------------------------------
# External IMPORTS
# -------------------------------------
source "$(dirname "${0}")/../utils/install-utils.zsh"

export function config_clamav() {

    log "🛡️ Setting up ClamAV..."

    # Stop existing CLAMAV services
    sudo systemctl stop clamav-clamonacc.service clamav-daemon.service clamav-freshclam.service

    # Ensure CLAMAV user and shadow group exist
    if ! getent group shadow &>/dev/null; then
        sudo groupadd shadow
    fi

    if ! id -u clamav &>/dev/null; then
        sudo useradd -r -s /usr/bin/nologin clamav
    fi

    # PERMISSIONS
    sudo chown root:shadow /etc/shadow && sudo chmod 640 /etc/shadow

    # Create REQUIRED DIRECTORIES and set permissions
    sudo install -d -o clamav -g clamav -m 755 /var/lib/clamav /var/log/clamav /var/run/clamav /root/quarantine
    if [[ ! -d "${HOME}/quarantine" ]]; then
        mkdir -p ~/quarantine
    fi
    sudo chown -R clamav:clamav ~/quarantine

    # FRESHCLAM log
    sudo touch /var/log/clamav/freshclam.log
    sudo chown clamav:clamav /var/log/clamav/freshclam.log
    sudo chmod 644 /var/log/clamav/freshclam.log

    # SYSTEMD user override
    mkdir -p "${HOME}/.config/systemd/user"
    cat <<EOF >~/.config/systemd/user/clamav-clamonacc.service
    [Unit]
    Description=ClamAV On-Access Scanner
    After=clamav-daemon.service

    [Service]
    ExecStart=/usr/bin/clamonacc -F --fdpass --log=/var/log/clamav/clamonacc.log
    Restart=on-failure

    [Install]
    WantedBy=default.target
    EOF

    # ALLOW notifications
    if ! grep -q 'clamav ALL' "/etc/sudoers.d/clamav" &>/dev/null; then
        echo 'clamav ALL=(ALL) NOPASSWD: SETENV: /usr/bin/notify-send' | sudo tee "/etc/sudoers.d/clamav" >/dev/null
    fi

    # Enable and start CLAMAV services
    sudo systemctl enable --now clamav-daemon clamav-freshclam clamav-clamonacc.service
    sudo systemctl start clamav-daemon clamav-freshclam clamav-clamonacc.service
    sudo freshclam

    # Enable SCHEDULED TASKS
    sudo systemctl enable --now cronie.service

    return 0
}
