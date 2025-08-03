#!/usr/bin/env zsh
#
# -------------------------------------
# Install Common Tools for Arch Linux
# -------------------------------------

# Import Install Utils
source "$(dirname "${0}")/install-utils.zsh"

echo "🔄 Updating system..."
sudo pacman -Syu --noconfirm

# Define packages
PACMAN_PACKAGES=(
    # System Utilities
    base-devel tmux fd less man bat btop htop pydf tldr reflector stow
    ranger speedtest-cli openssh trash-cli fzf glances lsd ripgrep lazygit vivid
    kdiff3 httpie curl ncdu onefetch neofetch fastfetch cronie ufw clamav git-delta
    ueberzug wine fzf cargo gwenview system-config-printer transmission-cli transmission-gtk
    direnv sqlite jq wl-clipboard graphviz gnuplot maim scrot plantuml
    shfmt shellcheck tidy stylelint isync offlineimap xorg-xwininfo msmtp gnupg w3m
    haveged man man-pages man-db bc

    # GUI Applications
    virtualbox virtualbox-host-modules-arch virtualbox-guest-utils firefox thunderbird filezilla gimp
    libreoffice dbeaver steam discord obs-studio kdenlive gparted vlc
    qemu virt-manager virt-viewer dnsmasq vde2 bridge-utils openbsd-netcat libvirt edk2-ovmf

    # Development Tools
    vim neovim emacs make gcc clang cmake direnv maven gradle nodejs npm yarn spring
    jdk17-openjdk go ruby rust luarocks cabal-install kotlin clojure lighttpd php composer
)

# -------------------------------------
#  Install Packman Packages
# -------------------------------------
install_packman_packages "${PACMAN_PACKAGES}"

# -------------------------------------
# Dotfiles
# -------------------------------------
DOTFILES="../dotfiles"
echo "💾 Copying main config file to home root directory..."
if [[ -d "${DOTFILES}" ]]; then

    local CONFIG_DIR="${HOME}/.zshrc.d/config.d"

    backup_and_copy "${DOTFILES}/.zshrc" "${HOME}/.zshrc" false
    backup_and_copy "${CONFIG_DIR}/nvim" "${HOME}/.config/nvim" false
    backup_and_copy "${CONFIG_DIR}/tmux" "${HOME}/.config/tmux" false
    backup_and_copy "${CONFIG_DIR}/ranger" "${HOME}/.config/ranger" false
    backup_and_copy "${CONFIG_DIR}/clamav" "/etc/clamav" true
    backup_and_copy "${CONFIG_DIR}/cron/cron.daily" "/etc/cron.daily" true
    backup_and_copy "${CONFIG_DIR}/cron/cron.weekly" "/etc/cron.weekly" true
    backup_and_copy "${CONFIG_DIR}/ufw/before.rules" "/etc/ufw/before.rules" true

else
    echo "❌ Dotfiles directory not found. Skipping dotfile setup." &>2
fi

systemctl enable --now haveged
systemctl enable --now libvirtd
sudo usermod -aG libvirt $(whoami)
systemctl start libvirtd
systemctl enable vboxservice.service

# VBox drivers (only if using VirtualBox with Vagrant)
if lsmod | grep -q vboxdrv; then
    echo "📦 vboxdrv already loaded"
else
    echo "📦 Loading vboxdrv kernel module..."
    sudo modprobe vboxdrv || echo "⚠️ Failed to load vboxdrv. You may need to reboot or install kernel headers." >&2
fi

# -------------------------------------
# UFW Firewall Configuration
# -------------------------------------
echo "🔧 Configuring UFW firewall..."

sudo systemctl enable --now ufw
sudo ufw --force enable
sudo ufw allow http
sudo ufw allow https
sudo ufw deny 5900
sudo ufw limit 22/tcp
sudo ufw default allow outgoing
sudo ufw default deny incoming
sudo ufw logging high

# -------------------------------------
# ClamAV Configuration
# -------------------------------------
echo "🛡️ Setting up ClamAV..."

# Stop existing ClamAV services
sudo systemctl stop clamav-clamonacc.service clamav-daemon.service clamav-freshclam.service

# Ensure clamav user and shadow group exist
if ! getent group shadow &>/dev/null; then
    sudo groupadd shadow
fi

if ! id -u clamav &>/dev/null; then
    sudo useradd -r -s /usr/bin/nologin clamav
fi

# Permissions
sudo chown root:shadow /etc/shadow && sudo chmod 640 /etc/shadow

# Create required directories and set permissions
sudo install -d -o clamav -g clamav -m 755 /var/lib/clamav /var/log/clamav /var/run/clamav /root/quarantine
if [[ ! -d "${HOME}/quarantine" ]]; then
    mkdir -p ~/quarantine
fi
sudo chown -R clamav:clamav ~/quarantine

# Freshclam log
sudo touch /var/log/clamav/freshclam.log
sudo chown clamav:clamav /var/log/clamav/freshclam.log
sudo chmod 644 /var/log/clamav/freshclam.log

# Systemd user override
mkdir -p ~/.config/systemd/user
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

# Allow notifications
if ! grep -q 'clamav ALL' /etc/sudoers.d/clamav &>/dev/null; then
    echo 'clamav ALL=(ALL) NOPASSWD: SETENV: /usr/bin/notify-send' | sudo tee /etc/sudoers.d/clamav >/dev/null
fi

# Enable and start ClamAV services
sudo systemctl enable --now clamav-daemon clamav-freshclam clamav-clamonacc.service
sudo systemctl start clamav-daemon clamav-freshclam clamav-clamonacc.service
sudo freshclam

# Enable scheduled tasks
sudo systemctl enable --now cronie.service

# Change default shell to Zsh
chsh -s "$(which zsh)"
source ~/.zshrc
echo "⚙️ Zsh is now your default shell."

# Make all scripts executable
chmod +x ~/.zshrc.d/functions.d/*.zsh

echo -e "\n🎉 Setup complete. Your system is ready!"
