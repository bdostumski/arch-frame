#!/usr/bin/env bash
#
# BASH PROFILE / THEMES
# Description: Bash prompt, colors, fzf and direnv integration
#              Mirrors zsh profile.zsh structure
#

# -----------------
# TERMINAL THEME
# -----------------
# Notes:
# To configure starship prompt, run: starship init bash
# To switch to powerlevel10k-style prompt in bash, install starship + a Nerd Font
# ----------
# FIX: Themes live under .shell.d/config.d/themes — use SHELLDDIR not SHELLDIR
BASH_THEMES="${SHELLDDIR}/config.d/themes/bash"
LS_THEMES="${HOME}/.zshrc.d/config.d/themes/shell"

# ----------
# Setup terminal theme
TERMINAL_THEME='.pure_theme.sh'
# Prompt pure theme using (p10k plugin)
# [[ ! -f "${BASH_THEMES}/${TERMINAL_THEME}" ]] || source "${BASH_THEMES}/${TERMINAL_THEME}"

# -----------------
# LS COLOR SCHEMES
# -----------------
# Uncomment one to activate (mirrors the full list in profile.zsh)
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
# Apply LS color scheme
[[ -f "${LS_COLOR_SCHEME}" ]] && export LS_COLORS="$(cat "${LS_COLOR_SCHEME}")"

# -----------------
# COMPLETION STYLING
# Bash equivalent of zsh zstyle completion settings in profile.zsh
# -----------------
# Use LS_COLORS for colored completion list (mirrors zstyle list-colors)
bind 'set colored-stats on'
# Case-insensitive completion (mirrors zstyle matcher-list 'm:{a-z}={A-Z}')
bind 'set completion-ignore-case on'
# Show completions on single Tab even if ambiguous (mirrors zsh menu select)
bind 'set show-all-if-ambiguous on'
bind 'TAB: menu-complete'             # cycle through completions with Tab
bind '"\e[Z": menu-complete-backward' # shift+Tab to cycle backwards

# -----------------
# FZF INTEGRATION
# Must run before key-bindings are set (fzf --bash registers its own bindings)
# Mirrors: eval "$(fzf --zsh)" in profile.zsh
# -----------------
command -v fzf &>/dev/null && eval "$(fzf --bash)"

export FZF_DEFAULT_OPTS="--ansi --height=40% --layout=reverse --border"
export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window=down:3:wrap"
export FZF_CTRL_T_OPTS="--preview 'lsd --color=always {}' --preview-window=right:50%"
export FZF_ALT_C_OPTS="--preview 'lsd --color=always --tree {} | head -50'"

# -----------------
# DIRENV
# Mirrors: command -v direnv guard in profile.zsh
# -----------------
command -v direnv &>/dev/null && eval "$(direnv hook bash)"

# -----------------
# TERRAFORM AUTOCOMPLETE
# -----------------
command -v terraform &>/dev/null && complete -o nospace -C "$(which terraform)" terraform

# -----------------
# BLE.SH ATTACH
# Must be the VERY LAST line — attaches the line editor after everything is loaded
# Equivalent of zinit finishing plugin setup in zsh
# -----------------
[[ ${BLE_VERSION-} ]] && ble-attach
