#!/usr/bin/env sh
#
# -------------------------------------
# VBOX DRIVERS (only if using VirtualBox with Vagrant)
# -------------------------------------
#

# -------------------------------------
# Variables
# -------------------------------------
BASE_DIR="$(cd "$(dirname "${0}")" && pwd)"

# -------------------------------------
# External IMPORTS
# -------------------------------------
. "${BASE_DIR}/dependencies/utils/install-utils.sh"

config_vbox() {

    sudo systemctl enable --now haveged
    sudo systemctl enable --now libvirtd
    sudo usermod -aG libvirt "$(whoami)"
    sudo systemctl start libvirtd
    sudo systemctl enable vboxservice.service

    if lsmod | grep -q vboxdrv; then
        log "📦 vboxdrv already loaded"
    else
        log "📦 Loading vboxdrv kernel module..."
        sudo modprobe vboxdrv || log "⚠️ Failed to load vboxdrv. You may need to reboot or install kernel headers." >&2
    fi

    return 0
}
