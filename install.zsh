#!/usr/bin/env zsh

echo "\n"
echo " ▗▄▖ ▗▄▄▖  ▗▄▄▖▗▖ ▗▖    ▗▄▄▄▖▗▄▄▖  ▗▄▖ ▗▖  ▗▖▗▄▄▄▖ "
echo "▐▌ ▐▌▐▌ ▐▌▐▌   ▐▌ ▐▌    ▐▌   ▐▌ ▐▌▐▌ ▐▌▐▛▚▞▜▌▐▌    "
echo "▐▛▀▜▌▐▛▀▚▖▐▌   ▐▛▀▜▌    ▐▛▀▀▘▐▛▀▚▖▐▛▀▜▌▐▌  ▐▌▐▛▀▀▘ "
echo "▐▌ ▐▌▐▌ ▐▌▝▚▄▄▖▐▌ ▐▌    ▐▌   ▐▌ ▐▌▐▌ ▐▌▐▌  ▐▌▐▙▄▄▖ "
echo "\n"
echo "Let's try to master the chaos 🔥"
echo "Created by Borislav Aleksandrov Dostumski"
echo "\n"
echo "This is the first of its sort, Linux configuration framework 🛠️"
echo "But first you have to install and run zsh"
echo "\n"
echo "Highly recommended is to try it on a virtual machine, or to install it on a fresh Arch installation"
echo "\n"
echo "Install Dependencies 📦"
echo "0) Install dependencies neede for the script [Zsh, Kitty, Fonts, Python]"
echo "1) Install core dependencies [System, Security, Virtualization, Basic Editors]"
echo "2) Install Dev Tools [Docker, Vagrant, K8s, etc.]"
echo "3) Install as basic editor [LazyVim]"
echo "4) Install IDE for programming [Doom Emacs]"
echo "5) Install System Drivers & Firmware"
echo "x) Exit"
echo "\n"
echo -n "Enter your choise: "
read -r CHOICE
if [ "${CHOICE}" = "x" ]; then
    echo "Exiting..."
    exit 0
fi

DEPENDENCIES_PATH="$(dirname ${0})/dependencies"

case $CHOICE in
0)
    echo "\n"
    echo "= = = = = = = = = ="
    echo "Starting installation [Zsh, Kitty, Fonts, Python] . . ."
    source "${DEPENDENCIES_PATH}/pacman-packages-main.zsh"
    echo "💡 Exit default terminal, and run kitty terminal"
    ;;
1)
    echo "\n"
    echo "= = = = = = = = = ="
    echo "Starting installation [System, ClamAV, UFW, Wine, VirtualBox, Vim, Neovim, Emacs, etc] . . ."
    source "${DEPENDENCIES_PATH}/pacman-packages.zsh"
    source "${DEPENDENCIES_PATH}/git-packages.zsh"
    source "${DEPENDENCIES_PATH}/yay-packages.zsh"
    echo "💡 Restart [exit/start again] rerun the script with kitty terminal"
    ;;
2)
    echo "\n"
    echo "= = = = = = = = = ="
    echo "Starting installation [Docker, Vagrant, K8s, etc.] . . ."
    source "${DEPENDENCIES_PATH}/dev-tools.zsh"
    echo "💡 Restart [exit/start again] kitty terminal"
    ;;

3)
    echo "\n"
    echo "= = = = = = = = = ="
    echo "Starting installation [LazyVim] . . ."
    source "${DEPENDENCIES_PATH}/lazyvim.zsh"
    ;;
4)
    echo "\n"
    echo "= = = = = = = = = ="
    echo "Starting installation [Doom Emacs] . . ."
    source "${DEPENDENCIES_PATH}/doom-emacs.zsh"
    ;;
5)
    echo "\n"
    echo "= = = = = = = = = ="
    echo "Install System Drivers & Firmware . . ."
    source "${DEPENDENCIES_PATH}/drivers.zsh"
    ;;
*)
    echo "\n"
    echo -e "Invalid CHOICE. Please try again." >&2
    exit 1
    ;;
esac

exit 0
