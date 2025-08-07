#!/usr/bin/env zsh
#
# Pacman  packages
# Export packages for pacman-packages.zsh
#

export PACMAN_PACKAGES=(
    # System Utilities
    base-devel tmux fd less man bat btop htop pydf tldr reflector stow
    ranger speedtest-cli openssh trash-cli fzf glances lsd ripgrep lazygit vivid
    kdiff3 httpie curl ncdu onefetch neofetch fastfetch cronie ufw clamav git-delta
    ueberzug wine fzf cargo gwenview system-config-printer transmission-cli transmission-gtk
    direnv sqlite jq wl-clipboard graphviz gnuplot maim scrot plantuml
    shfmt shellcheck tidy stylelint isync offlineimap xorg-xwininfo msmtp gnupg w3m
    haveged man man-pages man-db bc

    # GUI Applications
    virtualbox virtualbox-host-modules-arch virtualbox-guest-utils firefox thunderbird filezilla gimp
    libreoffice dbeaver steam discord obs-studio kdenlive gparted vlc
    qemu virt-manager virt-viewer dnsmasq vde2 bridge-utils openbsd-netcat libvirt edk2-ovmf

    # Development Tools
    vim neovim emacs make gcc clang cmake direnv maven gradle nodejs npm yarn spring
    jdk17-openjdk go ruby rust luarocks cabal-install kotlin clojure lighttpd php composer
)
