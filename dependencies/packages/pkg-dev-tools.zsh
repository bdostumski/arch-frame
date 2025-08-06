#!/usr/bin/env zsh
#
# Development Tools Packages
#

# Export Packman Packages for Dev Tools
export PACMAN_PACKAGES=(
    docker
    docker-machine
    docker-compose
    ansible
    minikube
    kubeadm
    kubectl
    containerd
    helm
)

# Export yay packages for dev tools
export AUR_PACKAGES=(
    grip
    docker-compose
    dockfmt
    vagrant
    qemu-full
    libvirt
    virt-manager
    dnsmasq
)
