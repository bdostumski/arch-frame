#!/usr/bin/env zsh
#
# ZSH CONFIGURATION
# Description: Main zsh config — loads shared + zsh-specific modules
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Shell Home Directories
export SHELLDIR="${HOME}/.zshrc.d"
export SHELLDDIR="${HOME}/.shelld"

# Load zsh-specific initialization first (Zinit must come before plugins)
[[ -f "${SHELLDIR}/initialize.zsh" ]] && source "${SHELLDIR}/initialize.zsh"

# Load shared modules
[[ -f "${SHELLDDIR}/shell-detect.sh"  ]] && source "${SHELLDDIR}/shell-detect.sh"
[[ -f "${SHELLDDIR}/environment.sh"   ]] && source "${SHELLDDIR}/environment.sh"
[[ -f "${SHELLDDIR}/aliases.sh"       ]] && source "${SHELLDDIR}/aliases.sh"
[[ -f "${SHELLDDIR}/templates.sh"     ]] && source "${SHELLDDIR}/templates.sh"
[[ -f "${SHELLDDIR}/functions.sh"     ]] && source "${SHELLDDIR}/functions.sh"
[[ -f "${SHELLDDIR}/local.sh"         ]] && source "${SHELLDDIR}/local.sh"

# Load zsh-specific modules
[[ -f "${SHELLDIR}/history.zsh"      ]] && source "${SHELLDIR}/history.zsh"
[[ -f "${SHELLDIR}/key-bindings.zsh" ]] && source "${SHELLDIR}/key-bindings.zsh"
[[ -f "${SHELLDIR}/plugins.zsh"      ]] && source "${SHELLDIR}/plugins.zsh"
[[ -f "${SHELLDIR}/profile.zsh"      ]] && source "${SHELLDIR}/profile.zsh"
