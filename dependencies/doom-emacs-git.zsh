#!/usr/bin/env zsh
#
# ----------------------------------------------------------------------
# Install Doom Emacs
# ----------------------------------------------------------------------

# Import Install Utils
source "$(dirname "${0}")/install-utils.zsh"

echo "\n⚙️  Starting Doom Emacs installation...\n"

# -------------------------------
# Install Doom Emacs if needed
# -------------------------------
echo "📦 Cloning Doom Emacs..."
if git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs &>/dev/null; then
    echo "✅ Doom Emacs cloned."
else
    echo "❌ Doom Emacs already exists at ~/.config/emacs. Skipping clone."
    exit 1
fi

# -------------------------------------
# Dotfiles
# -------------------------
# Create systemd service
# -------------------------
echo "🛠️  Setting up systemd service for Emacs..."
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

echo "✅ Emacs systemd service created."

# -------------------------------
# Create basic offlinemaprc IMAP config
# -------------------------------
echo "💾 Writing offlineimaprc config..."
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
echo "✅ offlineimap config written."

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
echo "✅ msmtprc config written."

# -----------------------
# GPG encryption
# -----------------------
echo "🔒 Generate a GPG key..."
gpg --full-generate-key

#e cho "🔒 Register your mail clien..."
# firefox https://support.google.com/accounts/answer/185833

echo "🔐 Setup username and password (password should be without spaces generated from google) in .offlineimaprc "
vim ~/.offlineimaprc

echo "🔐 Setup username and password (password should be without spaces generated from google) in .msmtprc "
vim ~/.msmtprc

mu init --maildir=~/Maildir --my-address=b.dostumski@gmail.com
mu index

# -----------------------
# Emacs service start
# -----------------------
echo "📁 Backing up ~/.emacs.d (if any)..."
mv ~/.emacs.d ~/.emacs.d-bak
"✅ Backup created."

echo "🌀 Enabling and starting Emacs systemd service..."
systemctl --user daemon-reexec
systemctl --user daemon-reload
systemctl --user enable --now emacs.service
echo "✅ Emacs systemd service set up."

# ----------------------------------
# Link libtree-sitter if missing
# ----------------------------------
echo "\n🧪 Checking libtree-sitter..."
if [[ ! -f "/usr/lib/libtree-sitter.so.0.24" && -f "/usr/lib/libtree-sitter.so" ]]; then
    echo "🔗 Creating symbolic link for libtree-sitter..."
    sudo ln -s /usr/lib/libtree-sitter.so /usr/lib/libtree-sitter.so.0.24 &&
        echo "✅ libtree-sitter symlink created." ||
        echo "❌ Failed to create libtree-sitter symlink."
else
    echo "✅ libtree-sitter already properly linked or missing entirely." >&2
fi

# -------------------------------------
#echo "💾 Copying main config file to home root directory..."
if [[ -d "dotfiles" ]]; then
    backup_and_copy ~/.zshrc.d/config.d/doom ~/.config/doom
else
    echo "❌ Dotfiles directory not found. Skipping dotfile setup." >&2
fi

mkdir -p ~/Maildir
mkdir -p ~/Documents/doom/org/roam/

echo "🔧 Installing Doom Emacs..."
~/.config/emacs/bin/doom install

echo "🔄 Syncing Doom Emacs profiles..."
~/.config/emacs/bin/doom profile sync --all
~/.config/emacs/bin/doom sync --rebuild
echo "✅ Doom profiles synced and rebuilt."

echo "\n🎉 Setup complete!"
