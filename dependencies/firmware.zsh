#!/usr/bin/env zsh

# Define packages
packages=(

    # Printer
    cups system-config-printer foomatic-db foomatic-db-engine ghostscript
    
)

echo "📦 Installing ${#packages[@]} packages..."
for pkg in "${packages[@]}"; do
    echo -e "\n👉 Installing: \033[1m$pkg\033[0m"
    if ! pacman -Qi "$pkg" &>/dev/null; then
        if sudo pacman -S --needed --noconfirm "$pkg"; then
            echo -e "✅ \033[1m$pkg\033[0m installed."
        else
            echo -e "❌ Failed to install: \033[1m$pkg\033[0m"
        fi
    else
        echo -e "✅ \033[1m$pkg\033[0m is already installed."
    fi
done

# Setup printer

echo "✅ Enable cups"
sudo systemctl enable --now cups.service

echo "📦 Installing brother-dcp-l2510d drivers"
yay -S --noconfirm brother-dcp-l2510d

echo "✅ Restart cups"
sudo systemctl restart cups.service




