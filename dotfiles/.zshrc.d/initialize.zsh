#!/usr/bin/env zsh
#
# ZSH INITIALIZATION
# Description: Bootstrap Zinit plugin manager and create required directories
#

# -----------------
# LOGS CONFIGURATION
# -----------------
LOGS="${HOME}/.zshrc.d/.logs"
# Create .logs to store application messages
[[ ! -d "${LOGS}" ]] && mkdir -p "${LOGS}"

# -----------------
# HISTORY FILE
# -----------------
# Create history file if it does not exist
[[ ! -f "${LOGS}/.zsh_history" ]] && touch "${LOGS}/.zsh_history"

# -----------------
# AUTOSUGGESTION CONFIGURATION
# -----------------
export ZSH_AUTOSUGGEST_STRATEGY=(history completion)                 # set autosuggest strategy (history, completion, match_prev_cmd)
export ZSH_AUTOSUGGEST_HISTORY_IGNORE="(ls|pwd|exit|sudo reboot)"    # ignore these commands in autosuggest history (ls|pwd|exit|sudo reboot|cd|cd -|cd ..)
export ZSH_AUTOSUGGEST_COMPLETION_IGNORE="(ls|pwd|exit|sudo reboot)" # ignore these commands in autosuggest completion (ls|pwd|exit|sudo reboot|cd|cd -|cd ..)
export ZSH_IGNORE_ALL_DUPS=1                                         # ignore all duplicates in history

# -----------------
# ZINIT CONFIGURATION
# -----------------
export ZINIT[PLUGINS_DIR]="${HOME}/.config/zinit/share/zinit/plugins"
export ZINIT[SNIPPETS_DIR]="${HOME}/.config/zinit/share/zinit/snippets"
export ZINIT_HOME="${HOME}/.config/zinit"
export ZINIT[COMPINIT_OPTS]=-C
export ZINIT[ZCOMET_NO_UPDATE]=1
export ZINIT_DEFAULT_PROTOCOL="SSH"

ZINIT_GIT="${HOME}/.config/zinit/.zinit.git"
# ----------
# Download Zinit, if not already installed
if [[ ! -d "${ZINIT_GIT}" ]]; then
    mkdir -p "$(dirname ${ZINIT_GIT})"
    git clone https://github.com/zdharma-continuum/zinit.git "${ZINIT_GIT}"
fi

# ----------
# Plugin Manager
source "${ZINIT_GIT}/zinit.zsh"
# ----------
# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zdharma-continuum/zinit-annex-as-monitor \
    zdharma-continuum/zinit-annex-bin-gem-node \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-rust
