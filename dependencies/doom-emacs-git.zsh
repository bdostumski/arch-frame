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

echo "💾 Create ~/.authinfo file..."
cat <<EOF >~/.authinfo
machine smtp.gmail.com login b.dostumski@gmail.com password your_app_password port 587
EOF
echo "✅ Please edit ~/.authinfo file with your own data."

# -------------------------------
# Create basic mbsyncrc config
# -------------------------------
echo "💾 Writing mbsyncrc config..."
cat <<EOF >~/.mbsyncrc
IMAPAccount gmail
Host imap.gmail.com
User b.dostumski@gmail.com
PassCmd "gpg -q --for-your-eyes-only --no-tty -d ~/.mailpw.gpg"
SSLType IMAPS
AuthMechs LOGIN

IMAPStore gmail-remote
Account gmail

MaildirStore gmail-local
Path ~/Documents/doom/mail/gmail/
Inbox ~/Documents/doom/mail/gmail/Inbox
Flatten .

Channel gmail
Far :gmail-remote:
Near :gmail-local:
Patterns *
Create Near
Sync All
EOF

echo "✅ mbsyncrc config written."

# -----------------------
# GPG encryption
# -----------------------
echo "🔒 Generate a GPG key..."
gpg --full-generate-key

echo "🔐 Encrypt .authinfo with GPG"
gpg -e -r b.dostumski@gmail.com ~/.authinfo

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
    echo "✅ libtree-sitter already properly linked or missing entirely."
fi

# -------------------------------------
#echo "💾 Copying main config file to home root directory..."
if [[ -d "dotfiles" ]]; then
    backup_and_copy ~/.zshrc.d/config.d/doom ~/.config/doom
else
    echo "❌ Dotfiles directory not found. Skipping dotfile setup."
fi

mkdir -p ~/Documents/doom/mail/gmail/{cur,new,tpm,Sent,Trash,Drafts,Archive}
mkdir -p ~/Documents/doom/org/roam/

echo "🔧 Installing Doom Emacs..."
~/.config/emacs/bin/doom install

echo "🔄 Syncing Doom Emacs profiles..."
~/.config/emacs/bin/doom profile sync --all
~/.config/emacs/bin/doom sync --rebuild
echo "✅ Doom profiles synced and rebuilt."

echo "\n🎉 Setup complete!"
