#!/usr/bin/env sh
#
# ----------------------------------------------------------------------
# Install YAY packages
# ----------------------------------------------------------------------
#

# -------------------------------------
# External IMPORTS
# -------------------------------------
SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
. "${SCRIPT_DIR}/dependencies/utils/install-utils.sh"
. "${SCRIPT_DIR}/dependencies/packages/pkg-yay.sh"

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
