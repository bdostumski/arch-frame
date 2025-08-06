#!/usr/bin/env zsh
#
# Main pacman packages
#

export PACMAN_PACKAGES=(

    # Linux kernel
    linux-zen
    linux-zen-headers

    # Fonts
    ttf-dejavu ttf-liberation ttf-roboto ttf-ubuntu-font-family noto-fonts
    noto-fonts-emoji noto-fonts-cjk ttf-fira-code ttf-fira-mono ttf-fira-sans
    ttf-jetbrains-mono ttf-hack ttf-inconsolata nerd-fonts ttf-opensans

    # Terminal Fonts
    terminus-font

    # System Utilities
    kitty zsh git github-cli ranger

    # Python + tools
    python python-pip python-pipenv python-virtualenv python-pynvim pyenv
)
