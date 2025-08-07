#!/usr/bin/env zsh
#
# -------------------------------
# Install DOOM EMACS
# -------------------------------
#

# -------------------------------
# External IMPORTS
# -------------------------------
source "$(dirname "${0}")/utils/install-utils.zsh"

log "\n⚙️  Starting Doom Emacs installation...\n"

# -------------------------------
# Install DOOM EMACS
# -------------------------------
log "📦 Cloning Doom Emacs..."
if git clone --depth 1 "https://github.com/doomemacs/doomemacs" "${HOME}/.config/emacs" &>/dev/null; then
    log "✅ Doom Emacs cloned."
else
    log "❌ Doom Emacs already exists at ~/.config/emacs. Skipping clone."
    exit 1
fi

# -------------------------
# Create SYSTEMD service
# -------------------------
log "🛠️  Setting up systemd service for Emacs..."
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

# -------------------------------
# Create offlinemaprc IMAP config
# -------------------------------
log "💾 Writing offlineimaprc config..."
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

chmod 600 "${HOME}/.offlineimap"
log "✅ offlineimap config written."

# -------------------------------
# Create msmtprc SMTP config
# -------------------------------
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

# -----------------------
# GPG encryption
# -----------------------
log "🔒 Generate a GPG key..."
gpg --full-generate-key

# echo "🔒 Register your mail clien..."
# firefox https://support.google.com/accounts/answer/185833

log "🔐 Setup username and password (password should be without spaces generated from google) in .offlineimaprc "
vim "${HOME}/.offlineimaprc"

log "🔐 Setup username and password (password should be without spaces generated from google) in .msmtprc "
vim "${HOME}/.msmtprc"

mu init --maildir="${HOME}/Maildir" --my-address=b.dostumski@gmail.com
mu index

# -----------------------
# Start EMACS service
# -----------------------
log "📁 Backing up ~/.emacs.d (if any)..."
move_file "${HOME}/.emacs.d"
if [[ "${?}" -eq 0 ]]; then
    "✅ Backup created."
fi

log "🌀 Enabling and starting Emacs systemd service..."
systemctl --user daemon-reexec
systemctl --user daemon-reload
systemctl --user enable --now emacs.service
log "✅ Emacs systemd service set up."

# ----------------------------------
# Link LIBTREE-SITTER if missing
# ----------------------------------
log "\n🧪 Checking libtree-sitter..."
if [[ ! -f "/usr/lib/libtree-sitter.so.0.24" && -f "/usr/lib/libtree-sitter.so" ]]; then
    log "🔗 Creating symbolic link for libtree-sitter..."
    sudo ln -s /usr/lib/libtree-sitter.so /usr/lib/libtree-sitter.so.0.24 &&
        log "✅ libtree-sitter symlink created." ||
        log "❌ Failed to create libtree-sitter symlink."
else
    log "✅ libtree-sitter already properly linked or missing entirely." ">&2"
fi

# -------------------------
# Copy and backup DOTFILES
# -------------------------
log "💾 Copying main config file to home root directory..."
if [[ -d "dotfiles" ]]; then
    backup_and_copy "${HOME}/.zshrc.d/config.d/doom" "${HOME}/.config/doom"
else
    log "❌ Dotfiles directory not found. Skipping dotfile setup." ">&2"
fi

mkdir -p "${HOME}/Maildir"
mkdir -p "${HOME}/Documents/doom/org/roam/"

log "🔧 Installing Doom Emacs..."
"${HOME}/.config/emacs/bin/doom install"

log "🔄 Syncing Doom Emacs profiles..."
"${HOME}/.config/emacs/bin/doom profile sync --all"
"${HOME}/.config/emacs/bin/doom sync --rebuild"
log "✅ Doom profiles synced and rebuilt."

# -------------------------------------
# DONE
# -------------------------------------
log "\n🎉 All setup steps completed!"
