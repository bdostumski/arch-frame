#!/usr/bin/env sh
#
# ----------------------------------------------------------------------
# Install & Configure DEVELOPMENT TOOLS on Arch Linux
# ----------------------------------------------------------------------
#

# ---------------------------
# External IMPORTS
# ---------------------------
SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
. "${SCRIPT_DIR}/dependencies/utils/install-utils.sh"
. "${SCRIPT_DIR}/dependencies/packages/pkg-dev-tools.sh"

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
case "${MACHINE_TYPE}" in
desktop | workstation | laptop)
    log "🔧 Configuring Libvirt..."
    sudo systemctl enable --now libvirtd
    sudo usermod -aG libvirt "${USER}"
    ;;
server)
    log "ℹ️  Skipping Libvirt on server profile (use remote VMs instead)."
    ;;
esac

# ---------------------------
# MINIKUBE configuration
# ---------------------------
case "${MACHINE_TYPE}" in
desktop | workstation | laptop)
    log "🔧 Configuring MiniKube..."
    minikube start --driver=docker
    ;;
server)
    log "ℹ️  Skipping Minikube on server profile."
    ;;
esac

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
