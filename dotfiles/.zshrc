#!/usr/bin/env zsh
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
export SHELLDIR="$HOME/.zshrc.d"
# ----------
# load shell initialization
[ -f "$SHELLDIR/initialize.zsh" ] && source "$SHELLDIR/initialize.zsh"

# -----------------
# SHELL CONFIGURATION FILES
# -----------------
# load common enviroment variables
[ -f "$SHELLDIR/environment.zsh" ] && source "$SHELLDIR/environment.zsh"
# ----------
# load aliases
[ -f "$SHELLDIR/aliases.zsh" ] && source "$SHELLDIR/aliases.zsh"
# ----------
# load template files
[ -f "$SHELLDIR/templates.zsh" ] && source "$SHELLDIR/templates.zsh"
# ----------
# load functions
[ -f "$SHELLDIR/functions.zsh" ] && source "$SHELLDIR/functions.zsh"
# ----------
# load history settings
[ -f "$SHELLDIR/history.zsh" ] && source "$SHELLDIR/history.zsh"
# ----------
# load key bindings
[ -f "$SHELLDIR/key-bindings.zsh" ] && source "$SHELLDIR/key-bindings.zsh"
# ----------
# load plugins
[ -f "$SHELLDIR/plugins.zsh" ] && source "$SHELLDIR/plugins.zsh"
# ----------
# custom profile settings
[ -f "$SHELLDIR/profile.zsh" ] && source "$SHELLDIR/profile.zsh"
# ----------
# load local machine-specific configuration (optional)
# this file is for settings that should not be shared between machines
[ -f "$SHELLDIR/local.zsh" ] && source "$SHELLDIR/local.zsh"
