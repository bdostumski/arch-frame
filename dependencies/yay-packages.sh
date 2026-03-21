#!/usr/bin/env bash
#
# ----------------------------------------------------------------------
# Install YAY packages
# ----------------------------------------------------------------------
#

# -------------------------------------
# External IMPORTS
# -------------------------------------
. "$(dirname "${0}")/utils/install-utils.sh"
. "$(dirname "${0}")/packages/pkg-yay.sh"

# -------------------------------------
#  Install AUR packages
# -------------------------------------
install_yay_packages "${AUR_PACKAGES[@]}"


# -------------------------------------
#  Initialize Anaconda AI/ML platform for python
# -------------------------------------
if [ -d "/opt/anaconda/" ]; then
	conda init
fi

# -------------------------------------
# DONE
# -------------------------------------
log "\n🎉 All setup steps completed!"

# Suggestions (manual install/configuration may be required):
# yay -S --noconfirm aur/intellij-idea-ultimate-edition  # Java IDE (Ultimate)
# yay -S nemu  # TUI for QEMU, supports Kitty graphics protocol (requires config)
