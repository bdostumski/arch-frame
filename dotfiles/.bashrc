#!/usr/bin/env bash
#
# BASH CONFIGURATION
# Description: Main bash config — loads shared + bash-specific modules
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Shell Home Directories
export SHELLDIR="${HOME}/.bashrc.d"
export SHELLDDIR="${HOME}/.shelld"

# Load shared: environment, aliases, templates, functions, local
[[ -f "${SHELLDDIR}/shell-detect.sh"  ]] && source "${SHELLDDIR}/shell-detect.sh"
[[ -f "${SHELLDDIR}/environment.sh"   ]] && source "${SHELLDDIR}/environment.sh"
[[ -f "${SHELLDDIR}/aliases.sh"       ]] && source "${SHELLDDIR}/aliases.sh"
[[ -f "${SHELLDDIR}/templates.sh"     ]] && source "${SHELLDDIR}/templates.sh"
[[ -f "${SHELLDDIR}/functions.sh"     ]] && source "${SHELLDDIR}/functions.sh"
[[ -f "${SHELLDDIR}/local.sh"         ]] && source "${SHELLDDIR}/local.sh"

# Load bash-specific modules
[[ -f "${SHELLDIR}/initialize.bash"   ]] && source "${SHELLDIR}/initialize.bash"
[[ -f "${SHELLDIR}/history.bash"      ]] && source "${SHELLDIR}/history.bash"
[[ -f "${SHELLDIR}/key-bindings.bash" ]] && source "${SHELLDIR}/key-bindings.bash"
[[ -f "${SHELLDIR}/profile.bash"      ]] && source "${SHELLDIR}/profile.bash"
