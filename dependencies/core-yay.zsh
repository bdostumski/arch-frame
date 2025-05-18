#!/usr/bin/env zsh

# Install yay packages one by one with status icons

packages=(
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
    js-beautify mu lombok-common
)

echo "\n🔧 Starting installation of AUR packages...\n"

for pkg in "${packages[@]}"; do
    echo "📦 Installing: $pkg"
    if yay -Qi "$pkg" &>/dev/null; then
        echo "✅ Already installed: $pkg"
    elif yay -S --noconfirm "$pkg" &>/dev/null; then
        echo "✅ Success: $pkg installed"
    else
        echo "❌ Failed: $pkg installation failed"
    fi
    echo ""
done

echo "🏁 All packages processed."

# Suggestions (manual install/configuration may be required):
# yay -S --noconfirm aur/intellij-idea-ultimate-edition  # Java IDE (Ultimate)
# yay -S nemu  # TUI for QEMU, supports Kitty graphics protocol (requires config)
