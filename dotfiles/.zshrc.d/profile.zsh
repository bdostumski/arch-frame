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
ZSH_THEMES="${SHELLDDIR}/config.d/themes/ls"
LS_THEMES="${SHELLDIR}/config.d/themes/shell"

# ---------- 
# Setup terminal theme
TERMINAL_THEME='.pure_theme.sh'
# Prompt pure theme using (p10k plugin)
[[ ! -f "${ZSH_THEMES}/${TERMINAL_THEME}" ]] || source "${ZSH_THEMES}/${TERMINAL_THEME}"

# -----------------
# LS COLOR SCHEMES
# -----------------
# ls color scheme names
# LS_COLOR_SCHEME="${LS_THEMES}/alabaster_dark"
# LS_COLOR_SCHEME="${LS_THEMES}/ayu"
# LS_COLOR_SCHEME="${LS_THEMES}/catppuccin-frappe"
# LS_COLOR_SCHEME="${LS_THEMES}/catppuccin-latte"
# LS_COLOR_SCHEME="${LS_THEMES}/catppuccin-mocha"
# LS_COLOR_SCHEME="${LS_THEMES}/dracula"
# LS_COLOR_SCHEME="${LS_THEMES}/gruvbox-dark"
# LS_COLOR_SCHEME="${LS_THEMES}/gruvbox-dark-hard"
# LS_COLOR_SCHEME="${LS_THEMES}/gruvbox-dark-soft"
# LS_COLOR_SCHEME="${LS_THEMES}/gruvbox-light"
# LS_COLOR_SCHEME="${LS_THEMES}/gruvbox-light-hard"
# LS_COLOR_SCHEME="${LS_THEMES}/gruvbox-light-soft"
LS_COLOR_SCHEME="${LS_THEMES}/iceberg-dark"
# LS_COLOR_SCHEME="${LS_THEMES}/jellybeans"
# LS_COLOR_SCHEME="${LS_THEMES}/lava"
# LS_COLOR_SCHEME="${LS_THEMES}/modus-operandi"
# LS_COLOR_SCHEME="${LS_THEMES}/molokai"
# LS_COLOR_SCHEME="${LS_THEMES}/nord"
# LS_COLOR_SCHEME="${LS_THEMES}/one-dark"
# LS_COLOR_SCHEME="${LS_THEMES}/one-light"
# LS_COLOR_SCHEME="${LS_THEMES}/rose-pine"
# LS_COLOR_SCHEME="${LS_THEMES}/rose-pine-dawn"
# LS_COLOR_SCHEME="${LS_THEMES}/rose-pine-moon"
# LS_COLOR_SCHEME="${LS_THEMES}/snazzy"
# LS_COLOR_SCHEME="${LS_THEMES}/solarized-dark"
# LS_COLOR_SCHEME="${LS_THEMES}/solarized-light"
# LS_COLOR_SCHEME="${LS_THEMES}/tokyonight-moon"
# LS_COLOR_SCHEME="${LS_THEMES}/tokyonight-night"
# LS_COLOR_SCHEME="${LS_THEMES}/tokyonight-storm"
# LS_COLOR_SCHEME="${LS_THEMES}/zenburn"
# ---------- 
# Choose ls color scheme

# ---------- 
# Setup vivid plugin color scheme | Else use default custom color scheme
[[ -f "${LS_COLOR_SCHEME}" ]] && export LS_COLORS="$(cat "${LS_COLOR_SCHEME}")"

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
