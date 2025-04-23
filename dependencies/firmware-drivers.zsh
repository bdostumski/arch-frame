#!/usr/bin/env zsh
set -e

# System update
echo "🔄 System update..."
sudo pacman -Syu --noconfirm
command -v yay >/dev/null || {
    echo "❌ yay not found. Please install yay (AUR helper) first."
    exit 1
}
yay -Syu --noconfirm

# -------------------------------------
# Define Packages
# -------------------------------------
packages=(
    nvidia-dkms nvidia-utils nvidia-settings linux-zen-headers
    cuda linux-firmware-qlogic xf86-video-amdgpu
)

aur_packages=(
    cudnn nvidia-docker
)

# Install official packages
echo "📦 Installing ${#packages[@]} official packages..."
for pkg in "${packages[@]}"; do
    echo "👉 Installing: $pkg"
    if ! pacman -Qi "$pkg" &>/dev/null; then
        if ! sudo pacman -S --noconfirm --needed "$pkg"; then
            echo "❌ Failed to install: $pkg"
        fi
    else
        echo "✅ $pkg already installed."
    fi
done

# Install AUR packages
echo "📦 Installing ${#aur_packages[@]} AUR packages..."
for aur in "${aur_packages[@]}"; do
    echo "👉 Installing AUR: $aur"
    yay -S --noconfirm "$aur" || echo "❌ Failed to install AUR package: $aur"
done

# -------------------------------------
# NVIDIA MODULES (only if NVIDIA installed)
# -------------------------------------
echo "⚙️ Configuring mkinitcpio for NVIDIA..."

# Add NVIDIA modules to mkinitcpio config
sudo sed -i 's/^MODULES=.*/MODULES=(nvidia nvidia_modeset nvidia_uvm nvidia_drm amdgpu)/' /etc/mkinitcpio.conf

# Rebuild initramfs
sudo mkinitcpio -P

# Configure GRUB for NVIDIA DRM KMS
echo "⚙️ Configuring GRUB for NVIDIA DRM KMS..."
sudo sed -i 's/^GRUB_CMDLINE_LINUX_DEFAULT=.*/GRUB_CMDLINE_LINUX_DEFAULT="loglevel=3 quiet nvidia-drm.modeset=1"/' /etc/default/grub
sudo grub-mkconfig -o /boot/grub/grub.cfg

# Load NVIDIA modules
echo "⚙️ Loading NVIDIA modules..."
sudo modprobe nvidia
sudo modprobe nvidia_modeset
sudo modprobe nvidia_uvm
sudo modprobe nvidia_drm

# Multiple GPUs
sudo cat <<EOF >/etc/X11/xorg.conf.d/20-nvidia.conf
Section "Device"
    Identifier "NVIDIA"
    Driver "nvidia"
    Option "AllowEmptyInitialConfiguration" "true"
EndSection
EOF

# -------------------------------------
# CUDA + cuDNN Setup
# -------------------------------------
echo "⚙️ Linking cuDNN libs into CUDA dirs..."
for file in /usr/lib/libcudnn*; do
    [[ -e "$file" ]] && sudo ln -sf "$file" /opt/cuda/lib64/
done

for file in /usr/include/cudnn*; do
    [[ -e "$file" ]] && sudo ln -sf "$file" /opt/cuda/include/
done

# Set CUDA env vars persistently
env_file=~/.zshrc.d/environment.zsh
grep -q "CUDA" "$env_file" 2>/dev/null || {
    echo 'export PATH=/opt/cuda/bin:$PATH' >>"$env_file"
    echo 'export LD_LIBRARY_PATH=/opt/cuda/lib64:$LD_LIBRARY_PATH' >>"$env_file"
}

# -------------------------------------
# Python ML Stack
# -------------------------------------
echo "🐍 Installing PyTorch + TensorFlow..."
pip install --upgrade pip
pip install torch torchvision torchaudio --index-url https://download.pytorch.org/whl/cu121
pip install tensorflow

# Reload environment variables
source ~/.zshrc

echo "\n🎉 Done! Reboot to apply NVIDIA + GRUB settings."
