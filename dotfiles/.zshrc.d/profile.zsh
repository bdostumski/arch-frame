#!/usr/bin/env zsh
#
# CUSTOM CONFIGURATIONS & THEMES
#

# -----------------
# TERMINAL THEME 
# -----------------
# Notes:
# To create new prompt configuration, run `p10k configure`
# ---------- 
# Theme home directory path
THEMES="$SHELLDIR/config.d/themes"
# ---------- 
# Setup terminal theme
PURE_THEME='.pure_theme.sh'
# Prompt pure theme using (p10k plugin)
[[ ! -f "$THEMES/shell/$PURE_THEME" ]] || source "$THEMES/shell/$PURE_THEME"

# -----------------
# LS COLOR SCHEMES
# -----------------
# ls color scheme names
# LS_COLOR_SCHEME="$THEMES/ls/alabaster_dark"
# LS_COLOR_SCHEME="$THEMES/ls/ayu"
# LS_COLOR_SCHEME="$THEMES/ls/catppuccin-frappe"
# LS_COLOR_SCHEME="$THEMES/ls/catppuccin-latte"
# LS_COLOR_SCHEME="$THEMES/ls/catppuccin-mocha"
# LS_COLOR_SCHEME="$THEMES/ls/dracula"
# LS_COLOR_SCHEME="$THEMES/ls/gruvbox-dark"
# LS_COLOR_SCHEME="$THEMES/ls/gruvbox-dark-hard"
# LS_COLOR_SCHEME="$THEMES/ls/gruvbox-dark-soft"
# LS_COLOR_SCHEME="$THEMES/ls/gruvbox-light"
# LS_COLOR_SCHEME="$THEMES/ls/gruvbox-light-hard"
# LS_COLOR_SCHEME="$THEMES/ls/gruvbox-light-soft"
LS_COLOR_SCHEME="$THEMES/ls/iceberg-dark"
# LS_COLOR_SCHEME="$THEMES/ls/jellybeans"
# LS_COLOR_SCHEME="$THEMES/ls/lava"
# LS_COLOR_SCHEME="$THEMES/ls/modus-operandi"
# LS_COLOR_SCHEME="$THEMES/ls/molokai"
# LS_COLOR_SCHEME="$THEMES/ls/nord"
# LS_COLOR_SCHEME="$THEMES/ls/one-dark"
# LS_COLOR_SCHEME="$THEMES/ls/one-light"
# LS_COLOR_SCHEME="$THEMES/ls/rose-pine"
# LS_COLOR_SCHEME="$THEMES/ls/rose-pine-dawn"
# LS_COLOR_SCHEME="$THEMES/ls/rose-pine-moon"
# LS_COLOR_SCHEME="$THEMES/ls/snazzy"
# LS_COLOR_SCHEME="$THEMES/ls/solarized-dark"
# LS_COLOR_SCHEME="$THEMES/ls/solarized-light"
# LS_COLOR_SCHEME="$THEMES/ls/tokyonight-moon"
# LS_COLOR_SCHEME="$THEMES/ls/tokyonight-night"
# LS_COLOR_SCHEME="$THEMES/ls/tokyonight-storm"
# LS_COLOR_SCHEME="$THEMES/ls/zenburn"
# ---------- 
# Choose ls color scheme

# ---------- 
# Setup vivid plugin color scheme | Else use default custom color scheme
[[ -f "$LS_COLOR_SCHEME" ]] && export LS_COLORS="$(cat "$LS_COLOR_SCHEME")"

# -----------------
# FZF CONFIGURATIONS
# -----------------
# Completion styling
# COMPLETION_LIST_COLORS="di=36:fi=0:ln=34:mh=00:pi=33:so=35:do=35:bd=33;1:cd=33;1:or=31;1:mi=0:su=31:sg=31:ca=31:tw=32:ow=32:st=34;1:ex=92"
# ----------
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS} # Use LS_COLORS for completion list colors
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' # Case-insensitive completion
zstyle ':completion:*' menu no select # Disable menu selection
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls --color $realpath' # Preview files with ls
# ---------- 
# fzf shell integration
eval "$(fzf --zsh)"
# 
eval "$(direnv hook zsh)"
