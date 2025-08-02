#!/usr/bin/env bash
#
# ZSH CONFIGURATION
# Description: This file is the main configuration file for zsh. It loads all
#

# -----------------
# ZSH SHELL CONFIGURATION
# -----------------
# If not running shell interactively, don't do anyting
[[ $- != *i* ]] && return

# -----------------
# Initialize SHELL
# -----------------
# Shell Home Directory
export SHELLDIR="${HOME}/.zshrc.d"
# ----------
# load shell initialization
[[ -f "${SHELLDIR}/initialize.zsh" ]] && source "${SHELLDIR}/initialize.zsh"

# -----------------
# SHELL CONFIGURATION FILES
# -----------------
# load common enviroment variables
[[ -f "${SHELLDIR}/environment.zsh" ]] && source "${SHELLDIR}/environment.zsh"
# ----------
# load aliases
[[ -f "${SHELLDIR}/aliases.zsh" ]] && source "${SHELLDIR}/aliases.zsh"
# ----------
# load template files
[[ -f "${SHELLDIR}/templates.zsh" ]] && source "${SHELLDIR}/templates.zsh"
# ----------
# load functions
[[ -f "${SHELLDIR}/functions.zsh" ]] && source "${SHELLDIR}/functions.zsh"
# ----------
# load history settings
[[ -f "${SHELLDIR}/history.zsh" ]] && source "${SHELLDIR}/history.zsh"
# ----------
# load key bindings
[[ -f "${SHELLDIR}/key-bindings.zsh" ]] && source "${SHELLDIR}/key-bindings.zsh"
# ----------
# load plugins
[[ -f "${SHELLDIR}/plugins.zsh" ]] && source "${SHELLDIR}/plugins.zsh"
# ----------
# custom profile settings
[[ -f "${SHELLDIR}/profile.zsh" ]] && source "${SHELLDIR}/profile.zsh"
# ----------
# load local machine-specific configuration (optional)
# this file is for settings that should not be shared between machines
[[ -f "${SHELLDIR}/local.zsh" ]] && source "${SHELLDIR}/local.zsh"

### Added by Zinit's installer
ZINIT_INSTALLER="${HOME}/.local/share/zinit/zinit.git/zinit.zsh"
if [[ ! -f "${ZINIT_INSTALLER}" ]]; then
    print -P "%F{33} %F{220}Installing %F{33}ZDHARMA-CONTINUUM%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
    command mkdir -p "$HOME/.local/share/zinit" && command chmod g-rwX "$HOME/.local/share/zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.local/share/zinit/zinit.git" && \
        print -P "%F{33} %F{34}Installation successful.%f%b" || \
        print -P "%F{160} The clone has failed.%f%b"
fi

source "${ZINIT_INSTALLER}"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zdharma-continuum/zinit-annex-as-monitor \
    zdharma-continuum/zinit-annex-bin-gem-node \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-rust

### End of Zinit's installer chunk

# Terraform autocomplete
if command -v terraform &>/dev/null; then
  complete -o nospace -C "$(which terraform)" terraform
fi
