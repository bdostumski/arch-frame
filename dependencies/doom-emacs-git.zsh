#!/usr/bin/env zsh

source "$(dirname "$0")/install-utils.zsh"

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
# -------------------------------------
echo "💾 Copying main config file to home root directory..."
if [[ -d "dotfiles" ]]; then
    backup_and_copy ~/.zshrc.d/config.d/doom ~/.config/doom
else
    echo "❌ Dotfiles directory not found. Skipping dotfile setup."
fi

mkdir -p ~/Documents/org/roam/

echo "🔧 Installing Doom Emacs..."
~/.config/emacs/bin/doom install

echo "🔄 Syncing Doom Emacs profiles..."
~/.config/emacs/bin/doom profile sync --all
~/.config/emacs/bin/doom sync --rebuild
echo "✅ Doom profiles synced and rebuilt."

# -------------------------
# Create systemd service
# -------------------------
echo "🛠️  Setting up systemd service for Emacs..."
mkdir -p ~/.config/systemd/user

cat <<EOF >~/.config/systemd/user/emacs.service
[Unit]
Description=Emacs text editor
Documentation=info:emacs man:emacs(1) https://gnu.org/software/emacs/

[Service]
Type=forking
ExecStart=/usr/bin/emacs --daemon
ExecStop=/usr/bin/emacsclient --eval "(kill-emacs)"
Environment=SSH_AUTH_SOCK=%t/keyring/ssh
Restart=on-failure

[Install]
WantedBy=default.target
EOF

echo "✅ Emacs systemd service created."

# -------------------------------
# Create basic mbsyncrc config
# -------------------------------
echo "💾 Writing mbsyncrc config..."
cat <<EOF >~/.mbsyncrc
IMAPAccount gmail
Host imap.gmail.com
User your@gmail.com
PassCmd "gpg -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg"
SSLType IMAPS

IMAPStore gmail-remote
Account gmail

MaildirStore gmail-local
Path ~/.mail/gmail/
Inbox ~/.mail/gmail/INBOX

Channel gmail
Master :gmail-remote:
Slave :gmail-local:
Patterns *
Create Both
Sync All
Expunge Both
EOF

echo "✅ mbsyncrc config written."

# -----------------------
# Emacs service start
# -----------------------
echo "📁 Backing up ~/.emacs.d (if any)..."
mv ~/.emacs.d ~/.emacs.d-bak
"✅ Backup created."

echo "🌀 Enabling and starting Emacs systemd service..."
systemctl enable --now emacs.service
systemctl restart emacs.service
systemctl status emacs.service
systemctl stop emacs.service
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
    echo "✅ libtree-sitter already properly linked or missing entirely."
fi

echo "\n🎉 Setup complete!"
