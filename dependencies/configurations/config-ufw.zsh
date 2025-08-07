#!/usr/bin/env zsh
#
# -------------------------------------
# UFW FIREWALL configuration
# -------------------------------------
#

# -------------------------------------
# External IMPORTS
# -------------------------------------
source "$(dirname "${0}")/../utils/install-utils.zsh"

export function config_ufw() {

    log "🔧 Configuring UFW firewall..."

    sudo systemctl enable --now ufw
    sudo ufw --force enable
    sudo ufw allow http
    sudo ufw allow https
    sudo ufw deny 5900
    sudo ufw limit 22/tcp
    sudo ufw default allow outgoing
    sudo ufw default deny incoming
    sudo ufw logging high

    return 0
}
