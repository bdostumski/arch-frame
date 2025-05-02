#!/usr/bin/env zsh

# Define packages
packages=(

    # Linux kernel
    linux-zen linux-zen-headers

    # Printer
    cups system-config-printer foomatic-db foomatic-db-engine ghostscript

    # Nvidia
    nvidia nvidia-utils nvidia-settings 
    cuda
    
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

# Setup Nvidia
echo "✅ Setup Nvidia"
echo 'export PATH=/opt/cuda/bin:$PATH' >> ~/.zshrc.d/environment.zsh
echo 'export LD_LIBRARY_PATH=/opt/cuda/lib64:$LD_LIBRARY_PATH' >> ~/.zshrc.d/environment.zsh
source ~/.zshrc

# System setup
sudo systemctl enable systemd-oomd --now # Enable the Out-of-Memory daemon for better memory management:






