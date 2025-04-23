#!/usr/bin/env zsh
#
# FIND DIRECTORIES AND OPEN SELECTED IN NVIM
# Description: This script uses fd to find directories and open the selected
# Usage: find-dir <search dir>
#

find_dir() {
  fd -g -H -t d "$1" |
    fzf --preview "lsd -al --color=auto {}" \
      --bind "enter:execute(nvim {})"
}
