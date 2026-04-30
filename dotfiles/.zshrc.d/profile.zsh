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
ZSH_THEMES="${SHELLDIR}/config.d/themes/shell"
LS_THEMES="${SHELLDDIR}/config.d/themes/ls"

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

# Apply LS color scheme (if present)
[[ -f "${LS_COLOR_SCHEME}" ]] && export LS_COLORS="$(<"${LS_COLOR_SCHEME}")"

# ----------
# SETUP TERMINAL THEME (Powerlevel10k)
# ----------
# TERMINAL_THEME='.classic_theme.sh'
TERMINAL_THEME='.pure_theme.sh'

# Use Powerlevel10k config file if it exists. This prevents startup warnings
# when dotfiles are only partially installed.
if [[ -n "${ZSH_THEMES}" && -f "${ZSH_THEMES}/${TERMINAL_THEME}" ]]; then
  source "${ZSH_THEMES}/${TERMINAL_THEME}"   # ✅ source instead of export
else
  typeset -g POWERLEVEL9K_DISABLE_CONFIGURATION_WIZARD=true
fi

# -----------------
# FZF CONFIGURATIONS
# -----------------
# Completion styling
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}          # Use LS_COLORS for completion list colors
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'            # Case-insensitive completion
zstyle ':completion:*' menu no select                          # Disable menu selection
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls --color $realpath' # Preview files with ls

# fzf shell integration (only if installed)
command -v fzf >/dev/null 2>&1 && eval "$(fzf --zsh)"

# direnv integration (only if installed)
command -v direnv >/dev/null 2>&1 && eval "$(direnv hook zsh)"
