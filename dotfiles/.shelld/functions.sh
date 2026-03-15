#!/usr/bin/env sh
#
# FUNCTIONS
# Description: custom shell scripts
# Path: ~/.shelld/functions.d/
#

CURRENT_DIR="${SHELLDDIR}/functions.d"

# -----------------
# ARCHIVE EXTRATION
# -----------------
# Usage: extract <file name>
. "${CURRENT_DIR}/archive-extraction.sh"

# -----------------
# NAVIGATION
# -----------------
# Example: up 3
# This will navigate up 3 directories
. "${CURRENT_DIR}/change-dir-up.sh"

# -----------------
# FIND DIRECTORY
# -----------------
# Find directory and open selected in Nvim
. "${CURRENT_DIR}/find-dir.sh"

# -----------------
# FIND FILE
# -----------------
# Find file and open selected in Nvim
. "${CURRENT_DIR}/find-file.sh"

# -----------------
# RANGER
# -----------------
# Automatically change the current working directory after closing ranger
. "${CURRENT_DIR}/ranger-cd.sh"

# -----------------
# EDITOR MAIN LVIM
# -----------------
# If lvim exists then use lvim, else nvim, else vim
. "${CURRENT_DIR}/editor-lvim-nvim-vim.sh"

# -----------------
# EDITOR MAIN NVIM
# -----------------
# If nvim exists then use nvim, else vim
. "${CURRENT_DIR}/editor-nvim-vim.sh"
