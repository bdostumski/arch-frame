#!/usr/bin/env sh
#
# Development tools packages
# Export packages for dev-tools.zsh
#

export PACMAN_PACKAGES=(

    # -------------------------
    # Containerisation
    # -------------------------
    docker
    docker-machine
    containerd

    # -------------------------
    # Configuration Management
    # -------------------------
    ansible

    # -------------------------
    # Kubernetes
    # -------------------------
    minikube
    kubeadm
    kubectl

    # -------------------------
    # Helm — Kubernetes Package Manager
    # -------------------------
    helm
)

export AUR_PACKAGES=(

    # -------------------------
    # Container Utilities
    # -------------------------
    dockfmt-git

    # -------------------------
    # Virtual Machines
    # -------------------------
    vagrant
    qemu-full

    # -------------------------
    # Virtualisation Management
    # -------------------------
    libvirt
    virt-manager
    dnsmasq

    # -------------------------
    # Miscellaneous Dev Utilities
    # -------------------------
    grip # Markdown preview server
)
