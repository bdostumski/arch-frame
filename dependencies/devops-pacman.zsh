#!/usr/bin/env zsh
#
# ----------------------------------------------------------------------
# 🚀 Install & Configure DevOps Tools on Arch Linux
# ----------------------------------------------------------------------

set -e            # Exit on error
setopt no_nomatch # Avoid glob errors

# 🌀 Update system
echo "🔄 Updating system packages..."
sudo pacman -Syu --noconfirm

# 📦 DevOps tools from official repos
official_packages=(
    docker terraform ansible
    minikube kubeadm kubectl
    containerd helm virtualbox
)

echo "📥 Installing DevOps tools (official repos)..."
sudo pacman -S --needed --noconfirm "${official_packages[@]}"

# 📦 DevOps tools from AUR
aur_packages=(
    vagrant docker-compose
    vagrant-libvirt qemu libvirt virt-manager
    ebtables-nft dnsmasq
)

echo "📥 Installing DevOps tools (AUR)..."
for pkg in "${aur_packages[@]}"; do
    echo "📦 Installing AUR: $pkg"
    if yay -Qi "$pkg" &>/dev/null; then
        echo "✅ Already installed: $pkg"
    elif yay -S --noconfirm "$pkg"; then
        echo "✅ Installed: $pkg"
    else
        echo "❌ Failed to install: $pkg"
    fi
    echo ""
done

# Docker configuration
echo "🔧 Configuring Docker..."
sudo systemctl enable --now docker.service
sudo usermod -aG docker "$USER"

# Libvirt configuration
echo "🔧 Configuring libvirt..."
sudo systemctl enable --now libvirtd
sudo usermod -aG libvirt "$USER"

# Minikube setup
echo "🚀 Starting Minikube..."
minikube start --driver=docker || echo "⚠️ Minikube failed to start. Check Docker permissions."

# Helm repo setup (check if already added)
echo "🎯 Adding Helm repo..."
if ! helm repo list | grep -q "bitnami"; then
    helm repo add bitnami https://charts.bitnami.com/bitnami
    echo "✅ Added Helm repo: bitnami"
else
    echo "🎯 Bitnami Helm repo already exists, skipping."
fi

# Completion message
echo -e "\n🎉 All DevOps tools installed and configured successfully!"
echo "📌 Run: \`newgrp docker\` and \`newgrp libvirt\` or reboot to apply group changes."
echo -e "💡 Happy hacking! 🧑‍💻\n"
