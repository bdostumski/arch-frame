#!/usr/bin/env zsh
#
# ----------------------------------------------------------------------
# Install & Configure DevOps Tools on Arch Linux
# ----------------------------------------------------------------------

# Exit on error
set -e

# Avoid glob errors
setopt no_nomatch

# Import Install Utils
source "$(dirname "${0}")/install-utils.zsh"

# 🌀 Update system
log "🔄 Updating system packages..."
sudo pacman -Syu --noconfirm

# 📦 DevOps tools from official repos
PACMAN_PACKAGES=(
    docker docker-machine docker-compose ansible minikube kubeadm kubectl containerd helm
)

# -------------------------------------
#  Install Pacman Packages
# -------------------------------------
install_packman_packages "${PACMAN_PACKAGES}"

# 📦 DevOps tools from AUR
AUR_PACKAGES=(
    grip
    docker-compose dockfmt
    vagrant vagrant-libvirt qemu libvirt virt-manager
    ebtables-nft dnsmasq
)

# -------------------------------------
#  Install AUR Packages
# -------------------------------------
install_yay_packages "${AUR_PACKAGES}"

# Docker configuration
log "🔧 Configuring Docker..."
sudo systemctl enable --now docker.service
sudo usermod -aG docker "${USER}"

# Libvirt configuration
log "🔧 Configuring Libvirt..."
sudo systemctl enable --now libvirtd
sudo usermod -aG libvirt "${USER}"

# Minikube setup
log "🔧 Configuring MiniKube..."
minikube start --driver=docker

# Helm repo setup (check if already added)
log "🎯 Adding Helm repo..."
helm repo add bitnami https://charts.bitnami.com/bitnami
log "✅ Added Helm repo: bitnami"

log "📦 Should install terraform manually"

# Completion message
log "\n🎉 All DevOps tools installed and configured successfully!"
log "💡 Happy hacking! 🧑‍💻\n"
