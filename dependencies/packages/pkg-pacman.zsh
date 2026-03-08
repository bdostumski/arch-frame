#!/usr/bin/env zsh
#
# Pacman  packages
# Export packages for pacman-packages.zsh
#

export PACMAN_PACKAGES=(

    # -------------------------
    # Linux Kernel
    # -------------------------
    linux-zen
    linux-zen-headers

    # -------------------------
    # Fonts
    # -------------------------
    ttf-dejavu
    ttf-liberation
    ttf-roboto
    ttf-ubuntu-font-family
    noto-fonts
    noto-fonts-emoji
    noto-fonts-cjk
    ttf-fira-code
    ttf-fira-mono
    ttf-fira-sans
    ttf-jetbrains-mono
    ttf-hack
    ttf-inconsolata
    nerd-fonts
    ttf-opensans

    # -------------------------
    # Terminal Fonts
    # -------------------------
    terminus-font

    # -------------------------
    # Shell & Terminal
    # -------------------------
    kitty
    zsh
    git
    github-cli
    ranger

    # -------------------------
    # Python & Tools
    # -------------------------
    python
    python-pip
    python-pipenv
    python-virtualenv
    python-pynvim
    pyenv

    # -------------------------
    # Base Development Tools
    # -------------------------
    base-devel
    make
    gcc
    clang
    cmake

    # -------------------------
    # Editors & IDE Support
    # -------------------------
    vim
    neovim
    emacs
    emacs-apel
    emacs-haskell-mode
    emacs-lua-mode
    emacs-muse
    emacs-python-mode
    emacs-slime

    # -------------------------
    # Version Control
    # -------------------------
    lazygit
    git-delta
    kdiff3

    # -------------------------
    # System Utilities
    # -------------------------
    tmux
    fd
    less
    man
    man-pages
    man-db
    bat
    btop
    htop
    pydf
    tldr
    reflector
    stow
    speedtest-cli
    openssh
    trash-cli
    fzf
    glances
    lsd
    ripgrep
    vivid
    httpie
    curl
    ncdu
    onefetch
    neofetch
    fastfetch
    cronie
    ufw
    clamav
    ueberzug
    cargo
    gwenview
    bc
    unzip
    haveged
    pass
    wl-clipboard
    xorg-xwininfo

    # -------------------------
    # File & Data Management
    # -------------------------
    sqlite
    jq
    direnv
    ncdu

    # -------------------------
    # Linting & Formatting
    # -------------------------
    shfmt
    shellcheck
    tidy
    stylelint

    # -------------------------
    # Mail & Communication
    # -------------------------
    isync
    offlineimap
    msmtp
    w3m
    gnupg
    mu

    # -------------------------
    # Multimedia & Graphics
    # -------------------------
    graphviz
    gnuplot
    maim
    scrot
    plantuml
    transmission-cli
    transmission-gtk
    wine

    # -------------------------
    # Documentation
    # -------------------------
    pandoc
    languagetool
    hunspell
    hunspell-en_us

    # -------------------------
    # Programming Languages & Build Tools
    # -------------------------
    nodejs
    npm
    yarn
    go
    ruby
    rust
    luarocks
    cabal-install
    kotlin
    clojure
    leiningen
    libvips
    gopls

    # -------------------------
    # JVM / Java
    # -------------------------
    jdk11-openjdk
    jdk17-openjdk
    jdk21-openjdk
    maven
    gradle
    spring

    # -------------------------
    # Web & Backend
    # -------------------------
    lighttpd
    php
    composer

    # -------------------------
    # Python Dev Tools
    # -------------------------
    python-black
    python-pyflakes
    python-isort
    python-pytest
    python-nose

    # -------------------------
    # GUI Applications — Browsers & Mail
    # -------------------------
    firefox
    thunderbird

    # -------------------------
    # GUI Applications — Office & Productivity
    # -------------------------
    libreoffice
    filezilla

    # -------------------------
    # GUI Applications — Media & Design
    # -------------------------
    gimp
    obs-studio
    kdenlive
    vlc

    # -------------------------
    # GUI Applications — System Tools
    # -------------------------
    gparted
    system-config-printer
    dbeaver

    # -------------------------
    # GUI Applications — Entertainment
    # -------------------------
    steam
    discord

    # -------------------------
    # Virtualisation (QEMU / KVM / libvirt)
    # -------------------------
    qemu
    virt-manager
    virt-viewer
    dnsmasq
    vde2
    bridge-utils
    openbsd-netcat
    libvirt
    edk2-ovmf
    virtualbox
    virtualbox-host-modules-arch
    virtualbox-guest-utils
)
