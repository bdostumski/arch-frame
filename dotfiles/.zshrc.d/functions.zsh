#!/usr/bin/env zsh
#
# FUNCTIONS
# Description: custom shell scripts
# Path: ~/.zshrc.d/functions.d/
#

# -----------------
# ARCHIVE EXTRATION
# -----------------
# Usage: extract <file name>
. "$(dirname "$0")/functions.d/archive-extraction.zsh"

# -----------------
# NAVIGATION
# -----------------
# Example: cd-up 3
# This will navigate up 3 directories
. "$(dirname "$0")/functions.d/change-dir-up.zsh"

# -----------------
# FIND DIRECTORY
# -----------------
# Find directory and open selected in Nvim
. "$(dirname "$0")/functions.d/find-dir.zsh"

# -----------------
# FIND FILE
# -----------------
# Find file and open selected in Nvim
. "$(dirname "$0")/functions.d/find-file.zsh"

# -----------------
# RANGER
# -----------------
# Automatically change the current working directory after closing ranger
. "$(dirname "$0")/functions.d/ranger-cd.zsh"

# -----------------
# EDITOR MAIN LVIM
# -----------------
# If lvim exists then use lvim, else nvim, else vim
. "$(dirname "$0")/functions.d/editor-lvim-nvim-vim.zsh"

# -----------------
# EDITOR MAIN NVIM
# -----------------
# If nvim exists then use nvim, else vim
. "$(dirname "$0")/functions.d/editor-nvim-vim.zsh"
