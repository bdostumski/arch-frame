#!/usr/bin/env bash
#
# ----------------------------------------------------------------------
# Install & Configure DEVELOPMENT TOOLS on Arch Linux
# ----------------------------------------------------------------------
#

# ---------------------------
# External IMPORTS
# ---------------------------
. "$(dirname "${0}")/utils/install-utils.sh"
. "$(dirname "${0}")/packages/pkg-dev-tools.sh"

# ---------------------------
#  Install PACMAN Packages
# ---------------------------
install_pacman_packages "${PACMAN_PACKAGES[@]}"

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
