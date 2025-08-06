#!/usr/bin/env zsh
#
# ----------------------------------------------------------------------
# Install Doom Emacs
# ----------------------------------------------------------------------

# -------------------------------
# External Imports
# -------------------------------
source "$(dirname "${0}")/utils/install-utils.zsh"

log "\n⚙️  Starting Doom Emacs installation...\n"

# -------------------------------
# Install DOOM EMACS
# -------------------------------
log "📦 Cloning Doom Emacs..."
if git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs &>/dev/null; then
    log "✅ Doom Emacs cloned."
else
    log "❌ Doom Emacs already exists at ~/.config/emacs. Skipping clone."
    exit 1
fi

# -------------------------
# Dotfiles
# -------------------------

# -------------------------
# Create SYSTEMD service
# -------------------------
log "🛠️  Setting up systemd service for Emacs..."
mkdir -p ~/.config/systemd/user

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
# Create basic offlinemaprc IMAP config
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

chmod 600 ~/.offlineimap
log "✅ offlineimap config written."

# -------------------------------
# Create basic msmtprc SMTP config
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

chmod 600 ~/.msmtprc
log "✅ msmtprc config written."

# -----------------------
# GPG encryption
# -----------------------
log "🔒 Generate a GPG key..."
gpg --full-generate-key

# echo "🔒 Register your mail clien..."
# firefox https://support.google.com/accounts/answer/185833

log "🔐 Setup username and password (password should be without spaces generated from google) in .offlineimaprc "
vim ~/.offlineimaprc

log "🔐 Setup username and password (password should be without spaces generated from google) in .msmtprc "
vim ~/.msmtprc

mu init --maildir=~/Maildir --my-address=b.dostumski@gmail.com
mu index

# -----------------------
# Emacs service start
# -----------------------
log "📁 Backing up ~/.emacs.d (if any)..."
mv ~/.emacs.d ~/.emacs.d-bak
"✅ Backup created."

log "🌀 Enabling and starting Emacs systemd service..."
systemctl --user daemon-reexec
systemctl --user daemon-reload
systemctl --user enable --now emacs.service
log "✅ Emacs systemd service set up."

# ----------------------------------
# Link libtree-sitter if missing
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

# -------------------------------------
#log "💾 Copying main config file to home root directory..."
if [[ -d "dotfiles" ]]; then
    backup_and_copy ~/.zshrc.d/config.d/doom ~/.config/doom
else
    log "❌ Dotfiles directory not found. Skipping dotfile setup." ">&2"
fi

mkdir -p ~/Maildir
mkdir -p ~/Documents/doom/org/roam/

log "🔧 Installing Doom Emacs..."
~/.config/emacs/bin/doom install

log "🔄 Syncing Doom Emacs profiles..."
~/.config/emacs/bin/doom profile sync --all
~/.config/emacs/bin/doom sync --rebuild
log "✅ Doom profiles synced and rebuilt."

# -------------------------------------
# DONE
# -------------------------------------
log "\n🎉 All setup steps completed!"
