#!/usr/bin/env zsh
#
# FUNCTIONS
# Description: custom shell scripts
# Path: ~/.zshrc.d/functions.d/
#

# Relative path from this file to the others. Someting like ./other_file
CURRENT_DIR="$(dirname "$0")/functions.d"

# -----------------
# ARCHIVE EXTRATION
# -----------------
# Usage: extract <file name>
. "${CURRENT_DIR}/archive-extraction.zsh"

# -----------------
# NAVIGATION
# -----------------
# Example: up 3
# This will navigate up 3 directories
. "${CURRENT_DIR}/change-dir-up.zsh"

# -----------------
# FIND DIRECTORY
# -----------------
# Find directory and open selected in Nvim
. "${CURRENT_DIR}/find-dir.zsh"

# -----------------
# FIND FILE
# -----------------
# Find file and open selected in Nvim
. "${CURRENT_DIR}/find-file.zsh"

# -----------------
# RANGER
# -----------------
# Automatically change the current working directory after closing ranger
. "${CURRENT_DIR}/ranger-cd.zsh"

# -----------------
# EDITOR MAIN LVIM
# -----------------
# If lvim exists then use lvim, else nvim, else vim
. "${CURRENT_DIR}/editor-lvim-nvim-vim.zsh"

# -----------------
# EDITOR MAIN NVIM
# -----------------
# If nvim exists then use nvim, else vim
. "${CURRENT_DIR}/editor-nvim-vim.zsh"
