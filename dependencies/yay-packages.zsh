#!/usr/bin/env zsh
#
# ----------------------------------------------------------------------
# Install YAY packages
# ----------------------------------------------------------------------
#

# -------------------------------------
# External IMPORTS
# -------------------------------------
source "$(dirname "${0}")/utils/install-utils.zsh"
source "$(dirname "${0}")/packages/pkg-yay.zsh"

# -------------------------------------
#  Install AUR packages
# -------------------------------------
install_yay_packages "${AUR_PACKAGES[@]}"

# -------------------------------------
# DONE
# -------------------------------------
log "\n🎉 All setup steps completed!"

# Suggestions (manual install/configuration may be required):
# yay -S --noconfirm aur/intellij-idea-ultimate-edition  # Java IDE (Ultimate)
# yay -S nemu  # TUI for QEMU, supports Kitty graphics protocol (requires config)
