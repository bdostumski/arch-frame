#!/usr/bin/env sh
#
# ══════════════════════════════════════════════════════════════════════════════
# 1b. DEFAULT SHELL
# ══════════════════════════════════════════════════════════════════════════════
# Choose your default login shell after installation.
# Allowed values: "zsh", "bash", "fish"
#
# Example:
#   DEFAULT_SHELL="zsh"
#
DEFAULT_SHELL="zsh"
#
# ══════════════════════════════════════════════════════════════════════════════
# 1. OS USER
# ══════════════════════════════════════════════════════════════════════════════
# These values describe who you are on this machine.
# Your Linux system login username — the account you use to log in to Arch.
#
# REQUIRED — the installer will immediately abort if this is left empty.
#
# Example:
#   USER_NAME="johndoe"
#
USER_NAME="johndoe"
#
# ──────────────────────────────────────────────────────────────────────────────
# Your first (given) name.
# Used to compose your full name in:
#   • Git commits  →  git log --author
#   • .gitconfig   →  [user] name = "Borislav Dostumski"
#   • Doom Emacs   →  (setq user-full-name "Borislav Alexandrov Dostumski")
#
# REQUIRED — Git and Emacs configuration will fail without this.
#
# Example:
#   FIRST_NAME="John"
#
FIRST_NAME="John"
#
# ──────────────────────────────────────────────────────────────────────────────
# Your middle name — used only in Doom Emacs full name composition.
# Leave this empty ("") if you do not have a middle name.
#
# Optional — no part of the installer will fail if this is empty.
#
# Example:
#   MIDDLE_NAME="William"
#   MIDDLE_NAME=""          ← leave blank if not applicable
#
MIDDLE_NAME=""
#
# ──────────────────────────────────────────────────────────────────────────────
# Your last (family) name.
# Combined with FIRST_NAME to form your full name in Git and Emacs.
#
# Optional — but strongly recommended for correct Git authorship.
#
# Example:
#   LAST_NAME="Doe"
#
LAST_NAME="Doe"
#
# ══════════════════════════════════════════════════════════════════════════════
#  2. GIT & GITHUB
# ══════════════════════════════════════════════════════════════════════════════
# Your version control identity. This is written into ~/.gitconfig during
# installation and used for all Git operations and GitHub CLI interactions.
#
# Your GitHub (or GitLab) username — the handle you use to push code.
# Written into ~/.gitconfig in three places:
#   • [user]     username = "johndoe"   ← author identity
#   • [github]   user     = "johndoe"   ← GitHub CLI integration
#   • [sendmail] smtpuser = "johndoe"   ← Git send-email via SMTP
#
# REQUIRED — the installer will immediately abort if this is left empty.
#
# Example:
#   GIT_USER="johndoe"
#
GIT_USER="johndoe"
#
# ══════════════════════════════════════════════════════════════════════════════
#  3. EMAIL — GMAIL
# ══════════════════════════════════════════════════════════════════════════════
# Your Gmail credentials are used to configure a full local email setup
# inside Doom Emacs, including IMAP (receive) and SMTP (send) clients.
#
# The email stack installed is:
#   • offlineimap   — syncs Gmail inbox → ~/Maildir  (IMAP)
#   • msmtp         — sends outgoing mail via Gmail SMTP
#   • mu / mu4e     — indexes and reads mail inside Doom Emacs
#
# Your full Gmail address — the complete email you use to send and receive mail.
# Used in:
#   • ~/.gitconfig      →  [user] email = "b.dostumski@gmail.com"
#   • ~/.msmtprc        →  from = b.dostumski@gmail.com  (SMTP sender address)
#   • mu4e init         →  mu init --my-address="${GMAIL_EMAIL}"
#   • Doom Emacs        →  (setq user-mail-address "b.dostumski@gmail.com")
#
# REQUIRED — the installer will immediately abort if this is left empty.
#
# Example:
#   GMAIL_EMAIL="john.doe@gmail.com"
#
GMAIL_EMAIL="john.doe@gmail.com"
#
# ──────────────────────────────────────────────────────────────────────────────
# Your Gmail username — the part before @gmail.com.
# Used as the login identity for both IMAP and SMTP authentication.
# Used in:
#   • ~/.offlineimaprc  →  remoteuser = b.dostumski  (IMAP login)
#   • ~/.msmtprc        →  user       = b.dostumski  (SMTP login)
#
# Example:
#   GMAIL_USER="john.doe"
#
GMAIL_USER="john.doe"
#
# ──────────────────────────────────────────────────────────────────────────────
# Your Gmail App Password — NOT your regular Google account password.
# Google requires a dedicated App Password for third-party mail clients.
#
# How to generate a Gmail App Password:
#   1. Go to: https://myaccount.google.com/apppasswords
#   2. Select app: "Mail"  →  Select device: "Linux"
#   3. Click "Generate" and copy the 16-character password shown
#   4. Paste it below (spaces in the password are optional, they are ignored)
#
# Used in:
#   • ~/.offlineimaprc  →  remotepass = ••••••••  (IMAP authentication)
#   • ~/.msmtprc        →  password   = ••••••••  (SMTP authentication)
#
# Stored with chmod 600 — readable only by your user account.
#
# Example:
#   GMAIL_PASSWORD="abcd efgh ijkl mnop"
#
GMAIL_PASSWORD="$(echo "password" | gpg --symmetric --cipher-algo AES256 --armor | base64 -w0)"
#
# ══════════════════════════════════════════════════════════════════════════════
#  4. LOCAL DATABASE
# ══════════════════════════════════════════════════════════════════════════════
# Credentials for your local development database (PostgreSQL, MySQL, etc.).
# These are exported as shell environment variables so your applications,
# scripts, and Doom Emacs database tooling can connect without hardcoding
# credentials in your project files.
#
# All three database variables are OPTIONAL.
# You can update them at any time after installation in:
#    ~/.zshrc.d/config.d/env/.env.sh
#
# The name of your local development database.
# Used in:
#   • Shell environment  →  export DB_NAME="database"
#   • Doom Emacs         →  (getenv "DB_NAME") for database client integration
#
# Example:
#   DB_NAME="myapp_development"
#
DB_NAME="database"
#
# ──────────────────────────────────────────────────────────────────────────────
# The username that has access to the database defined in DB_NAME.
# Used in:
#   • Shell environment  →  export DB_USERNAME="user"
#   • Doom Emacs         →  (getenv "DB_USERNAME")
#
# Example:
#   DB_USERNAME="myapp_user"
#
DB_USERNAME="user"
#
# ──────────────────────────────────────────────────────────────────────────────
# The password for DB_USERNAME.
# Used in:
#   • Shell environment  →  export DB_PASSWORD="password"
#   • Doom Emacs         →  (getenv "DB_PASSWORD")
#
# Stored with chmod 600 — readable only by your user account.
#
# Example:
#   DB_PASSWORD="supersecret"
#
DB_PASSWORD="password"
#
# ──────────────────────────────────────────────────────────────────────────────
