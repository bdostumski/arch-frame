#!/usr/bin/env sh
#
# -------------------------------------
# VBOX DRIVERS (only if using VirtualBox with Vagrant)
# -------------------------------------
#

# -------------------------------------
# External IMPORTS
# -------------------------------------
. "$(dirname "${0}")/../utils/install-utils.sh"

config_vbox() {

    systemctl enable --now haveged
    systemctl enable --now libvirtd
    sudo usermod -aG libvirt $(whoami)
    systemctl start libvirtd
    systemctl enable vboxservice.service

    if lsmod | grep -q vboxdrv; then
        log "📦 vboxdrv already loaded"
    else
        log "📦 Loading vboxdrv kernel module..."
        sudo modprobe vboxdrv || log "⚠️ Failed to load vboxdrv. You may need to reboot or install kernel headers." >&2
    fi

    return 0
}
