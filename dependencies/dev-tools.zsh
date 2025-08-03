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
echo "🔄 Updating system packages..."
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
echo "🔧 Configuring Docker..."
sudo systemctl enable --now docker.service
sudo usermod -aG docker "${USER}"

# Libvirt configuration
echo "🔧 Configuring Libvirt..."
sudo systemctl enable --now libvirtd
sudo usermod -aG libvirt "${USER}"

# Minikube setup
echo "🔧 Configuring MiniKube..."
minikube start --driver=docker

# Helm repo setup (check if already added)
echo "🎯 Adding Helm repo..."
helm repo add bitnami https://charts.bitnami.com/bitnami
echo "✅ Added Helm repo: bitnami"

echo "📦 Should install terraform manually"

# Completion message
echo -e "\n🎉 All DevOps tools installed and configured successfully!"
echo -e "💡 Happy hacking! 🧑‍💻\n"
