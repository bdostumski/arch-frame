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

log "\n⚙️️  Starting Doom Emacs installation...\n"

# -------------------------------
# Install DOOM EMACS
# -------------------------------
log "📥 Cloning Doom Emacs..."
if git clone --depth 1 "https://github.com/doomemacs/doomemacs" "${HOME}/.config/emacs" &>/dev/null; then
    log "✅ Doom Emacs cloned."
else
    log "⚠️ Doom Emacs already exists at ~/.config/emacs. Skipping clone."
    exit 1
fi

# -------------------------
# MAIL CLIENT configuration
# -------------------------
log "Do you want MU4E configuration [y/n]:"
read -r MAIL

if [[ "${MAIL}" -eq 'y' ]]; then

    log "🛠️ Starting mail client configuration for doom-emacs ..."

    # -------------------------
    # Create SYSTEMD service
    # -------------------------
    config-doom-emacs-systemd

    # -----------------------
    # GPG encryption and register your MAIL CLIENT
    # -----------------------
    config-gpg-key

    # -------------------------------
    # Create OFFLINEMAPRC IMAP config
    # -------------------------------
    config-offlineimaprs-imap

    # -------------------------------
    # Create MSMTPRC SMTP config
    # -------------------------------
    config-msmtprc-smtp

    # -------------------------------
    # MU4E configuration
    # -------------------------------
    log "📨 Starting MU4E configuration"
    mu init --maildir="${HOME}/Maildir" --my-address=b.dostumski@gmail.com
    mu index

    log "✅ Mail client configuration for doom-emacs is done."

else

    log "⏭️ Skipped MAIL CLIENT configuration "

fi

# ----------------------------------
# Link LIBTREE-SITTER if missing
# ----------------------------------
log "\n🔍 Checking libtree-sitter..."
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
log "📂 Copying main config file to home root directory..."
if [[ -d "dotfiles" ]]; then
    backup_and_copy "${HOME}/.zshrc.d/config.d/doom" "${HOME}/.config/doom"
else
    log "⚠️ Dotfiles directory not found. Skipping dotfile setup." ">&2"
fi

mkdir -p "${HOME}/Maildir"
mkdir -p "${HOME}/Documents/doom/org/roam/"

log "🧩 Installing Doom Emacs..."
"${HOME}/.config/emacs/bin/doom install"

log "🔄 Syncing Doom Emacs profiles..."
"${HOME}/.config/emacs/bin/doom profile sync --all"
"${HOME}/.config/emacs/bin/doom sync --rebuild"
log "✅ Doom profiles synced and rebuilt."

# -------------------------------------
# DONE
# -------------------------------------
log "\n🎉 All setup steps completed!"
