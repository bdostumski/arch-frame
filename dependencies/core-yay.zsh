#!/usr/bin/env zsh
#
# ----------------------------------------------------------------------
# Install yay packages one by one with status icons
# ----------------------------------------------------------------------

# Import Install Utils
source "$(dirname "${0}")/install-utils.zsh"

AUR_PACKAGES=(
    # Code editor
    visual-studio-code-bin
    # API testing
    postman-bin
    # System optimizer
    stacer-bin
    # Messaging app
    viber
    # GUI backup system
    backintime
    # CLI backup system
    backintime-cli
    # Downgrade packages
    downgrade
    # Scala programming language
    scala
    # Scala language server
    metals
    # Elixir programming language
    # elixir
    # Haskell language server
    haskell-language-server
    # Java IDE
    intellij-idea-community-edition
    # Other
    js-beautify mu lombok-common auto-cpufreq
)

# -------------------------------------
#  Install AUR Packages
# -------------------------------------
install_yay_packages "${AUR_PACKAGES}"

log "🏁 All packages processed."

# Suggestions (manual install/configuration may be required):
# yay -S --noconfirm aur/intellij-idea-ultimate-edition  # Java IDE (Ultimate)
# yay -S nemu  # TUI for QEMU, supports Kitty graphics protocol (requires config)
