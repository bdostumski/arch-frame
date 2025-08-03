#!/usr/bin/env zsh
#
# FIND FILES AND OPEN SELECTED IN NVIM
# Description: This function uses fd to find files and open select one
# Usage: find-file <file name>
#

function find_file() {
  fd -g -H -t f "${1}" |
    fzf --preview "lsd -al --color=auto {}" \
      --bind "enter:execute(nvim {})"

  return 0
}
