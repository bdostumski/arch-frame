#!/bin/env zsh
#
# FILE TEMPLATES
# Description: File templates for various functionalities
# Path: ~/.zshrc.d/templates.d/
#

TEMPLATES_DIR="$(dirname "$0")/templates.d"

function templates() {

  printf "\n"
  printf "= = = = = = = = ="
  printf "\n"
  printf "T e m p l a t e s"
  printf "\n"
  printf "= = = = = = = = ="
  printf "\n"
  printf "\n1. Git"
  printf "\n2. Docker"
  printf "\n3. Docker Compose"
  printf "\n4. Kubernetes"
  printf "\n5. Vagrant"
  printf "\n0. Exit"
  printf "\n"
  printf "\nChoice: "
  read -r choice

  if [[ "$choice" = "0" ]]; then
    return
  fi

  case $choice in
  1)
    printf "\n"
    printf "= = = = = = = = ="
    printf "\n"
    printf "Git Templates"
    printf "\n"
    printf "= = = = = = = = ="
    printf "\n"
    printf "\n1. gitignore"
    printf "\n0. Exit"
    printf "\n "
    printf "\nChoice: "
    read -r git
    case $git in
    1)
      cat "$TEMPLATES_DIR/git/.gitignore" >>.gitignore
      ;;

    *)
      return
      ;;
    esac
    ;;

  2)
    printf "\n"
    printf "= = = = = = = = ="
    printf "\n"
    printf "Docker Templates"
    printf "\n"
    printf "= = = = = = = = ="
    printf "\n"
    printf "\n0. Exit"
    printf "\n"
    printf "\nChoice: "
    read -r docker
    case $docker in
    *)
      return
      ;;
    esac
    ;;

  3)
    printf "\n"
    printf "= = = = = = = = ="
    printf "\n"
    printf "Docker Compose Templates"
    printf "\n"
    printf "= = = = = = = = ="
    printf "\n"
    printf "\n0. Exit"
    printf "\n"
    printf "\nChoice: "
    read -r compose
    case $compose in
    *)
      return
      ;;
    esac
    ;;

  4)
    printf "\n"
    printf "= = = = = = = = ="
    printf "\n"
    printf "Kubernetes Templates"
    printf "\n"
    printf "= = = = = = = = ="
    printf "\n"
    printf "\n0. Exit"
    printf "\n"
    printf "\nChoice: "
    read -r kubernetes
    case $kubernetes in
    *)
      return
      ;;
    esac
    ;;

  5)
    printf "\n"
    printf "= = = = = = = = ="
    printf "\n"
    printf "Vagrant Templates"
    printf "\n"
    printf "= = = = = = = = ="
    printf "\n"
    printf "\n0. Exit"
    printf "\n"
    printf "\nChoice: "
    read -r vagrant
    case $vagrant in
    *)
      return
      ;;
    esac
    ;;

  *)
    return
    ;;
  esac

}
