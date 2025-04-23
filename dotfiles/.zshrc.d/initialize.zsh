#!/usr/bin/env zsh
#
# -----------------
# CREATE DIRECOTRIES & COPY CONFIG FILES
# -----------------

# -----------------
# LOGS CONFIGURATION
# -----------------
LOGS="$HOME/.logs.d"
# Create .logs.d to store application messages
[ ! -d "$LOGS" ] && mkdir -p "$LOGS"


# -----------------
# HISTORY FILE
# -----------------
# Create history file if it does not exist
[ ! -f "$LOGS/.zsh_history" ] && touch "$LOGS/.zsh_history"

# -----------------
# ZINIT CONFIGURATION
# -----------------
ZINIT_HOME="$HOME/.config/zinit/.zinit.git"
# ----------
# Download Zinit, if not already installed
if [ ! -d "$ZINIT_HOME" ]; then
    mkdir -p "$(dirname $ZINIT_HOME)"
    git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
fi

# ----------
# Plugin Manager
source "${ZINIT_HOME}/zinit.zsh"
# ----------
# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zdharma-continuum/zinit-annex-as-monitor \
    zdharma-continuum/zinit-annex-bin-gem-node \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-rust
