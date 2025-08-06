#!/usr/bin/env zsh
#
# ----------------------------------------------------------------------
# Install & Configure DevOps Tools on Arch Linux
# ----------------------------------------------------------------------

# ---------------------------
# External Imports
# ---------------------------
source "$(dirname "${0}")/utils/install-utils.zsh"
source "$(dirname "${0}")/packages/pkg-dev-tools.zsh"

# ---------------------------
#  Install PACMAN Packages
# ---------------------------
install_packman_packages "${PACMAN_PACKAGES[@]}"

# ---------------------------
#  Install AUR Packages
# ---------------------------
install_yay_packages "${AUR_PACKAGES[@]}"

# ---------------------------
# DOCKER configuration
# ---------------------------
log "🔧 Configuring Docker..."
sudo systemctl enable --now docker.service
sudo usermod -aG docker "${USER}"

# ---------------------------
# LIBVIRT configuration
# ---------------------------
log "🔧 Configuring Libvirt..."
sudo systemctl enable --now libvirtd
sudo usermod -aG libvirt "${USER}"

# ---------------------------
# MINIKUBE configuration
# ---------------------------
log "🔧 Configuring MiniKube..."
minikube start --driver=docker

# ---------------------------
# HELM repo setup
# ---------------------------
log "🎯 Adding Helm repo..."
helm repo add bitnami https://charts.bitnami.com/bitnami
log "✅ Added Helm repo: bitnami"

log "📦 Should install terraform manually"

# -------------------------------------
# DONE
# -------------------------------------
log "\n🎉 All DevOps tools installed and configured successfully!"
log "💡 Happy hacking! 🧑‍💻\n"
