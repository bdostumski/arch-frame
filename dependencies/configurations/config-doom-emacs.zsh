#!/usr/bin/env zsh
#
# -------------------------------
# DOOM EMACS configuration
# -------------------------------
#

# -------------------------------------
# External IMPORTS
# -------------------------------------
source "$(dirname "${0}")/../utils/install-utils.zsh"

# -------------------------
# Create SYSTEMD service
# -------------------------
export function config-doom-emacs-systemd() {

    log "🔁️️ Setting up systemd service for Emacs..."
    mkdir -p "${HOME}/.config/systemd/user"

    cat <<EOF >~/.config/systemd/user/emacs.service
[Unit]
Description=Emacs text editor (daemon)
Documentation=info:emacs man:emacs(1) https://gnu.org/software/emacs/
After=default.target

[Service]
Type=forking
ExecStart=/usr/bin/emacs --daemon
ExecStop=/usr/bin/emacsclient --eval "(kill-emacs)"
Restart=on-failure
Environment=SSH_AUTH_SOCK=%t/keyring/ssh

[Install]
WantedBy=default.target
EOF

    log "✅ Emacs systemd service created."

    log "📁 Backing up ~/.emacs.d (if any)..."
    move_file "${HOME}/.emacs.d"
    if [[ "${?}" -eq 0 ]]; then
        log "✅ Backup of Emacs created."
    fi

    log "🌀 Enabling and starting Emacs systemd service..."
    systemctl --user daemon-reexec
    systemctl --user daemon-reload
    systemctl --user enable --now emacs.service
    log "✅ Emacs systemd service set up."

    return 0
}

# -----------------------
# GPG encryption and register your MAIL CLIENT
# -----------------------
export function config-gpg-key() {

    log "🔐 Generate a GPG key..."
    gpg --full-generate-key

    echo "📧 Register your mail clien..."
    firefox https://support.google.com/accounts/answer/185833

    return 0
}

# -------------------------------
# Create OFFLINEMAPRC IMAP config
# -------------------------------
export function config-offlineimaprs-imap() {

    log "📬 Writing OFFLINEIMAPRC IMAP config..."

    cat <<EOF >~/.offlineimaprc
[general]
accounts = Gmail
maxsyncaccounts = 3

[Account Gmail]
localrepository = Local
remoterepository = Remote

[Repository Local]
type = Maildir
localfolders = ~/Maildir

[Repository Remote]
type = IMAP
remotehost = imap.gmail.com
remoteuser = YOUR_EMAIL
remotepass = YOUR_PASSWORD
ssl = yes
sslcacertfile = /etc/ssl/certs/ca-certificates.crt
maxconnections = 1
EOF

    chmod 600 "${HOME}/.offlineimaprc"
    log "✅ offlineimap config written."

    log "🔐 Setup username and password (password should be without spaces generated from google) in .offlineimaprc "
    vim "${HOME}/.offlineimaprc"

    log "✅ offlineimaprc configuration is done."

    return 0
}

# -------------------------------
# Create MSMTPRC SMTP config
# -------------------------------
export function config-msmtprc-smtp() {

    log "📤 Writing MSMTPRC SMTP config..."

    cat <<EOF >~/.msmtprc
defaults
auth           on
tls            on
tls_trust_file /etc/ssl/certs/ca-certificates.crt

account Gmail
host smtp.gmail.com
port 587
from YOUR_EMAIL
user YOUR_EMAIL
password YOUR_PASSWORD

account default : Gmail
EOF

    chmod 600 "${HOME}/.msmtprc"
    log "✅ msmtprc config written."

    log "🔐 Setup username and password (password should be without spaces generated from google) in .msmtprc "
    vim "${HOME}/.msmtprc"

    log "✅ msmtprc configuration is done."

    return 0
}
